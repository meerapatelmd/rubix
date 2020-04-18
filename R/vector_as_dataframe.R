#' Convert a vector to a dataframe
#' This function takes a vector and 
#' @param vector vector of any length with names
#' @importFrom dplyr tibble
#' @return dataframe
#' @examples
#'  x <- c("a", "b", "c")
#'  vector_as_dataframe(x) 
#' @export

vector_as_dataframe <-
        function(vector) {
                return(as.data.frame(t(vector)))
        }
