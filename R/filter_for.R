#' Filter for observations using a character vector of included values
#' @param filter_col column targeted for filtering
#' @param inclusion_vector a vector of values to be included
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at
#' @export

filter_for_vector <-
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
