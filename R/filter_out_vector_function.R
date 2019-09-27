#' Filter out observations using a character vector of excluded values
#' @param filter_col column targetted for filtering
#' @param exclusion_vector character vector of values to be excluded
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at

filter_out_vector <-
        function(dataframe, filter_col, exclusion_vector) {
                filter_col <- enquo(filter_col)

                dataframe %>%
                        filter_at(vars(!!filter_col), any_vars(grepl(paste(exclusion_vector, collapse = "|"), .) == FALSE))
        }
