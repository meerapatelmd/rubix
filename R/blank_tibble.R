#' @title 
#' Create a Blank Tibble
#' @description 
#' This function takes a character vector and creates a 0-row dataframe with the vector as the column name names.  
#' @param column_names vector of column names
#' @seealso 
#'  \code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#' @rdname blank_tibble
#' @export 
#' @importFrom purrr map reduce %>%

blank_tibble <-
        function(...) {
                
                Args <- unlist(rlang::list2(...))
                
                matrix(nrow = 1, 
                       ncol = length(Args),
                       dimnames = list(c(""), Args)) %>%
                        tibble::as_tibble() %>%
                        dplyr::slice(-1)
                
        }
