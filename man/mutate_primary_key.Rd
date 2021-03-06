% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated2.R
\name{mutate_primary_key}
\alias{mutate_primary_key}
\title{Adds primary key to dataframe in position 1}
\usage{
mutate_primary_key(
  dataframe,
  pkey_column_name,
  starting_number = NULL,
  width_left_pad_with_zero = NULL,
  prefix = NULL
)
}
\arguments{
\item{pkey_column_name}{name of pkey column}

\item{starting_number}{optional starting number for the primary key}

\item{width_left_pad_with_zero}{integer of the number of leading zeros desired. NULL by default.}

\item{prefix}{prefix desired with starting_number + left padding with zeros (if not NULL)}
}
\description{
Adds primary key to dataframe in position 1
}
\keyword{internal}
