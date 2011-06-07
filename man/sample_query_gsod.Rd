\name{sample_query_gsod}
\alias{sample_query_gsod}
\docType{data}
\title{Sample Query (GSOD)}
\description{
A sample query that pulls out mean wind speed and temperatures by day for the
Las Vegas/McCarran airport station in the NOAA GSOD dataset
}

\details{
  \preformatted{
    SELECT station_number,
    CONCAT(CONCAT(CONCAT(CONCAT(STRING(year), "/"),
      STRING(month)), "/"), STRING(day)) AS date,
    mean_wind_speed, mean_temp
    FROM [bigquery/samples/gsod]
    WHERE (station_number = 723860) AND (year >= 1990)
    ORDER BY date;}
}
