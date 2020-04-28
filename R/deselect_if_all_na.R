#' Deselect columns that are all NA
#' @importFrom dplyr select_if
#' @export


deselect_if_all_na <-
        function(dataframe) {
                
                all_is_na <- 
                        function(vector) {
                                return(all(is.na(vector)))
                        }
                
                all_is_not_na <-
                        function(vector) {
                                return(!(all_is_na(vector)))
                        }
                
                dataframe %>%
                        dplyr::select_if(all_is_not_na)
        }