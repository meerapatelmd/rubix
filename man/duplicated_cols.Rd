% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qa.R
\name{duplicated_cols}
\alias{duplicated_cols}
\title{Names of Duplicate Columns}
\usage{
duplicated_cols(data)
}
\description{
Return the column names in a dataframe that contain duplicate values of another column.
}
\examples{
library(tidyverse)
test_data <- 
        tibble(
                Group = sample(c("Apple", "Pear"), size = 10, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
                C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 10, replace = TRUE),
                D     = sample(c(NA, TRUE, FALSE), size = 10, replace = TRUE)
        ) \%>\%
        dplyr::mutate(E = B)
        

duplicated_cols(test_data)

test_data \%>\%
        select(-all_of(duplicated_cols(test_data)))
}
