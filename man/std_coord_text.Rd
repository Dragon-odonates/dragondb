% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{std_coord_text}
\alias{std_coord_text}
\title{Standardize well-known text}
\usage{
std_coord_text(coord)
}
\arguments{
\item{coord}{character vector containing well-known text
representation of coordinates}
}
\value{
A character vector where the output is standardized (to avoid
misplaced spaces)
}
\description{
Standardize well-known text
}
\details{
Converts the character vector to an object of class \code{sfc},
then back to character.
}
