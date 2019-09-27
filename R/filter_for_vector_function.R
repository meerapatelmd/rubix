#' Filter for observations using a character vector of included values
#' @param filter_col column targetted for filtering
#' @param inclusion_vector character vector of values to be included
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at

filter_in_vector <-
        function(dataframe, filter_col, inclusion_vector) {
                filter_col <- enquo(filter_col)

                dataframe %>%
                        filter_at(vars(!!filter_col), any_vars(grepl(paste(exclusion_vector, collapse = "|"), .) == TRUE))
        }
