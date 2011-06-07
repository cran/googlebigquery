# Copyright 2011 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Author: yolken@google.com (Benjamin Yolken)
#
# R interface to BigQuery service; see http://code.google.com/apis/bigquery/ for
# more details.
#
# Currently supports running queries and returning the results to R; may
# be extended in the future to also support writes back into Bigquery.

# Get the 100 most commonly occuring words in the Shakespeare corpus
sample_query_shakespeare <- "SELECT word, SUM(word_count) AS count FROM [bigquery/samples/shakespeare] GROUP BY word ORDER BY count DESC LIMIT 100;"

# Pull out mean wind speed and temperatures by day for the Las Vegas/McCarran
# airport station in the GSOD dataset. See the following URL for details:
# http://code.google.com/apis/bigquery/docs/dataset-gsod.html
sample_query_gsod <- "SELECT station_number, CONCAT(CONCAT(CONCAT(CONCAT(STRING(year), '/'), STRING(month)), '/'), STRING(day)) AS date, mean_wind_speed, mean_temp FROM [bigquery/samples/gsod] WHERE (station_number = 723860) AND (year >= 1990) ORDER BY date;"


bigquery_query <- function(
    query,
    auth_token,
    bigquery_url="https://www.googleapis.com/bigquery/v1",
    json_method="R") {
  # Run a Bigquery query and return the results as an R data frame.
  #
  # Args:
  #   query: A string containing a BigQuery query
  #   auth_token: A string containing a valid, ClientLogin, authorization
  #               token; see Bigquery documentation for instructions on getting
  #               this.
  #   bigquery_url: String containing Bigquery API endpoint
  #   json_method: Parsing method to pass to fromJSON function; 'C' is much
  #                faster than 'R' (the default), but doesn't work on older R
  #                versions.
  #
  # Returns:
  #   A data frame containing the query results or NULL if an error is
  #   detected
  curl <- getCurlHandle()

  header = basicHeaderGatherer()

  transformed_url <- paste(bigquery_url, '/query?q=', URLencode(query), sep='')

  string_result <- getURL(
      transformed_url,
      verbose=TRUE,
      ssl.verifypeer=FALSE,
      timeout=30,
      httpheader=c(
          "Authorization"=paste("GoogleLogin auth=", auth_token, sep=""),
          "User-Agent"="R client for Google BigQuery service"),
      headerfunction=header$update,
      curl=curl)

  json_result <- fromJSON(string_result, method=json_method)

  # If error occurs, print out result verbatim and return
  if (header$value()["status"] != "200") {
    cat(string_result)
    return(NULL)
  }

  num_columns <- length(json_result$data$fields)
  num_rows <- length(json_result$data$rows)

  # Convert JSON result from API into properly formatted R data frame
  column_names <- c()
  column_types <- c()
  column_values <- list()

  for (column in c(1:num_columns)) {
    column_names <- append(column_names, json_result$data$fields[[column]]$id)
    column_types <- append(column_types, json_result$data$fields[[column]]$type)
    column_values[[column]] <- c(1:num_rows)
  }

  for (row in c(1:num_rows)) {
    for (column in c(1:num_columns)) {
      # Convert result values to appropriate R types
      if (column_types[column] == "integer" ||
          column_types[column] == "float") {
        transformed_value <- (
            as.numeric(json_result$data$rows[[row]]$f[[column]]$v))
      } else {
        transformed_value <- json_result$data$rows[[row]]$f[[column]]$v
      }

      column_values[[column]][row] <- transformed_value
    }
  }

  result_frame <- data.frame(column_values)
  names(result_frame) <- column_names

  return(result_frame)
}
