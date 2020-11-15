#' @title
#' Select cols and convert them to a list
#' @description This function takes a dataframe and breaks it down into a list by column. The columns can be explicitly provided or if ... is empty, the entire dataframe will be broken down. 
#' @param id_column_name optional string of the column designated as the identifier that should remain in the output. 
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#' @rdname cols_to_list
#' @export 
#' @importFrom dplyr enquos select enquo %>%
#' @importFrom purrr map

cols_to_list <- 
        function(data, ...) {
                
                
               
                
                if (missing(...)) {
                        
                        
                        as.list(data)
                        
                
                } else {
                        
                        cols <- enquos(...)
                        
                        as.list(data %>%
                                        dplyr::select(!!!cols))
                        
                        
                }
                        
        }

#' List to Tibble
#' @importFrom tibble as_tibble
#' @export

list_to_tibble <- 
        function(list) {
                
                tibble::as_tibble(list)
                
        }
