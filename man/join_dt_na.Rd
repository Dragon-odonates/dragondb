% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{join_dt_na}
\alias{join_dt_na}
\title{Join data.tables on columns containing NAs}
\usage{
join_dt_na(dt1, dt2, cols1, cols2 = cols1, placeholder = "__NA__")
}
\arguments{
\item{dt1}{First data.table (primary data.table for join)}

\item{dt2}{Second data.table (secondary data.table)}

\item{cols1}{Columns used for the join in primary data.table}

\item{cols2}{Columns used for the join in secondary data.table}

\item{placeholder}{placeholder for the NAs}
}
\value{
The merge of the two data.tables, considering NA values
as a match.
}
\description{
Join data.tables on columns containing NAs
}
