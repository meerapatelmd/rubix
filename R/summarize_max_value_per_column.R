#' Summarizes each column with max value
#'
#' @param ... column names for vectors of any data class
#' @param na.rm TRUE if true NA are to be removed. Default is TRUE.
#' @importFrom dplyr mutate_at
#' @importFrom dplyr summarize_at
#'
summarize_max_value_per_column <-
        function(.data, ..., na.rm = TRUE) {
                max_value_vars <- enquos(...)

                return(
                        .data %>%
                                mutate_at(vars(!!!max_value_vars), funs(as.double(as.character(.)))) %>%
                                summarise_at(vars(!!!max_value_vars), max, na.rm = na.rm)
                )
        }
