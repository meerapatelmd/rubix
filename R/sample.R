#' Sample a dataframe for n obs and returns full dataframe if n > nrow
#' @param n number of observations
#' @param ... additional arguments for dplyr::sample_n function
#' @importFrom dplyr sample_n
#' @importFrom dplyr distinct
#' @export

sample_all_or_n <-
        function(data, n, replace = TRUE, ...) {
               
                if (nrow(data) <= n) {
                        
                       data
                        
                } else {
                        
                        dplyr::sample_n(
                                        tbl = data, 
                                        size = n, 
                                        replace = replace,
                                        ...
                                                )
                        
                        
                }
                
        }
