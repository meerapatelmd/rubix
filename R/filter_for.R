#' Filter a column for a set of values
#' @description This is a shortcut to using the dplyr::filter({col} %in% {vector}) function along with its inverse.
#' @param filter_col column targeted for filtering
#' @param inclusion_vector a vector of values to be included
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at
#' @export

filter_for <-
        function(dataframe, filter_col, inclusion_vector, invert = FALSE) {
                filter_col <- enquo(filter_col)
                
                if (invert) {
                        
                        dataframe %>%
                                dplyr::filter_at(vars(!!filter_col), any_vars(!(. %in% inclusion_vector)))
                        
                        
                } else {

                        dataframe %>%
                                dplyr::filter_at(vars(!!filter_col), any_vars(. %in% inclusion_vector))
                }
                
        }
