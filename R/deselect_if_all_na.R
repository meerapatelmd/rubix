#' @title
#' Deselect columns that are all NA
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname deselect_if_all_na
#' @export 
#' @importFrom dplyr select_if %>%


deselect_if_all_na <-
        function(.data) {
                
                all_is_na <- 
                        function(vector) {
                                return(all(is.na(vector)))
                        }
                
                all_is_not_na <-
                        function(vector) {
                                return(!(all_is_na(vector)))
                        }
                
                .data %>%
                        dplyr::select_if(all_is_not_na)
        }