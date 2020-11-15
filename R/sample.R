#' Sample a dataframe for n obs and returns full dataframe if n > nrow
#' @param data A dataframe or tibble.
#' @param n Number of rows to sample. If n is greater than the row count, the dataframe will be returned unchanged.
#' @param replace Sample with or without replacement?
#' @param weight Sampling weights. This must evaluate to a vector of non-negative numbers the same length as the input. Weights are automatically standardised to sum to 1.
#' @seealso 
#'  \code{\link[dplyr]{sample_n}}
#' @rdname sample_all_or_n
#' @example inst/examples/sample.R
#' @export 
#' @importFrom dplyr sample_n

sample_all_or_n <-
        function(data,  n, replace = FALSE, weight = NULL) {
               
                if (nrow(data) <= n) {
                        
                       data
                        
                } else {
                        
                        dplyr::sample_n(tbl = data, 
                                        size = n, 
                                        replace = replace,
                                        weight = weight)
                        
                        
                }
                
        }
