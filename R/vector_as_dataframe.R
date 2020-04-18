#' Convert a vector to a dataframe
#' This function takes a vector and returns a dataframe with one row of values provided by the vector. Column names are those of the vector or the generic names in cases of unnamed vectors.
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
