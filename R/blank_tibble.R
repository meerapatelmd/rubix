#' @title 
#' Create a Blank Tibble
#' @description 
#' This function takes a character vector and creates a 0-row dataframe with the vector as the column name names.  
#' @rdname blank_tibble
#' @export 
#' @importFrom rlang list2
#' @importFrom tibble as_tibble 
#' @importFrom dplyr slice

blank_tibble <-
        function(...) {
                
                Args <- unlist(rlang::list2(...))
                
                matrix(nrow = 1, 
                       ncol = length(Args),
                       dimnames = list(c(""), Args)) %>%
                        tibble::as_tibble() %>%
                        dplyr::slice(-1)
                
        }
