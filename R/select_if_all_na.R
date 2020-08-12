#' @title
#' Select columns that are all NA
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname select_if_all_na
#' @export 
#' @importFrom dplyr select_if %>%


select_if_all_na <-
        function(dataframe) {
                
                all_is_na <- 
                        function(vector) {
                                return(all(is.na(vector)))
                        }

                
                dataframe %>%
                        dplyr::select_if(all_is_na)
        }