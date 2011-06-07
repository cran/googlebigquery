\name{sample_query_shakespeare}
\alias{sample_query_shakespeare}
\docType{data}
\title{Sample Query (Shakespeare)}
\description{
A sample query that gets the 100 most commonly occuring words in the
Shakespeare corpus
}

\details{
  \preformatted{
    SELECT word, SUM(word_count) AS count
    FROM [bigquery/samples/shakespeare]
    GROUP BY word ORDER BY count DESC LIMIT 100;}
}
