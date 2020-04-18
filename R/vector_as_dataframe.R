#' Converts vector to a dataframe with names as column names
#' @param vector vector of any length with names
#' @importFrom dplyr tibble
#' @return dataframe
#' @examples 
#' x <- c("a", "b", "c")
#' vector_as_dataframe(x) 
#' @export

vector_as_dataframe <-
        function(vector) {
                return(dplyr::tibble(t(vector)))
        }
