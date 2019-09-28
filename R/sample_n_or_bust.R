#' Sample a dataframe for n obs and returns full dataframe if n > nrow
#' @param n number of observations
#' @param ... additional arguments for dplyr::sample_n function
#' @importFrom dplyr sample_n
#' @export

sample_n_or_bust <-
  function(dataframe, n, ...) {
    dplyr::sample_n(tbl = tbl, size = n, replace = TRUE, ...) %>%
                        distinct()
  }
