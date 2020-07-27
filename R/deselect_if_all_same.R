#' Deselect columns that are all NA
#' @importFrom dplyr select_if
#' @export


deselect_if_all_same <-
        function(dataframe) {
                
                all_is_same <- 
                        function(vector) {
                                length(unique(vector)) == 1
                        }
                
                all_is_not_same <-
                        function(vector) {
                                !all_is_same(vector)
                        }
                
                dataframe %>%
                        dplyr::select_if(all_is_not_same)
        }