% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated2.R
\name{cleanup_listnames}
\alias{cleanup_listnames}
\title{Standardize List Names
This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)}
\usage{
cleanup_listnames(list)
}
\description{
Standardize List Names
This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
}
\keyword{internal}
