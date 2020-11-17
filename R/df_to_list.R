#' @title
#' Dataframe to a List of Vectors
#' 
#' @description 
#' Convert a dataframe into a list of vectors for each column. If ... is empty, the entire dataframe will be broken down. 
#' @param data A dataframe or tibble.
#' @param ...  (optional) Columns to select for. 
#' @seealso 
#'  \code{\link[dplyr]{select}}
#' @rdname cols_to_list
#' @family dataframe <> list functions
#' @export 
#' @importFrom dplyr select

cols_to_list <- 
        function(data, ...) {
                
                
               
                
                if (missing(...)) {
                        
                        
                        as.list(data) %>%
                                purrr::map(~ unlist(.))
                        
                
                } else {
                        
                        cols <- enquos(...)
                        
                        as.list(data %>%
                                        dplyr::select(!!!cols))  %>%
                                purrr::map(~ unlist(.))
                        
                        
                }
                        
        }

#' List to Tibble
#' 
#' @description 
#' Convert a list of vectors into a dataframe. 
#' 
#' @rdname list_to_tibble
#' @family dataframe <> list functions
#' @importFrom tibble as_tibble
#' @export

list_to_tibble <- 
        function(list) {
                
                tibble::as_tibble(list)
                
        }
