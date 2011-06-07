\name{bigquery_query}
\alias{bigquery_query}
\title{Bigquery Query}
\description{
Run a Bigquery query, parse the result, and return an R data frame.
}

\usage{
bigquery_query(query, auth_token,
               bigquery_url="https://www.googleapis.com/bigquery/v1",
               json_method="R")
}

\arguments{
  \item{query}{A string containing a BigQuery query}
  \item{auth_token}{A string containing a valid, ClientLogin authorization token}
  \item{bigquery_url}{String containing Bigquery API endpoint;\cr
                      default: \code{"https://www.googleapis.com/bigquery/v1"}}
  \item{json_method}{Parsing method passed into \code{fromJSON} call;\cr
                     default: \code{"R"}}
}

\value{
Data frame containing results of query or NULL if error occurs
}

\author{Benjamin Yolken <yolken@google.com>}

\examples{
# Set auth token (replace X's with a real token string)
my_auth_token <- "XXXXXXX"

# Run query
result <- bigquery_query(sample_query_shakespeare, my_auth_token)

# Look at result
summary(result)
}

\keyword{google}
\keyword{bigquery}
