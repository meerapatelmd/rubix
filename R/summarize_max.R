#' Summarizes each column with max value
#' @param ... column names for vectors of any data class
#' @param na.rm TRUE if true NA are to be removed. Default is TRUE.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}}
#' @rdname summarize_max
#' @export 
#' @importFrom dplyr enquos %>%

summarize_max <-
        function(.data, ..., na.rm = TRUE) {
                max_value_vars <- dplyr::enquos(...)

                return(
                        .data %>%
                                mutate_at(vars(!!!max_value_vars), funs(as.double(as.character(.)))) %>%
                                summarise_at(vars(!!!max_value_vars), max, na.rm = na.rm)
                )
        }
