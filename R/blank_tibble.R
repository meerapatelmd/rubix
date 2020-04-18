#' Create a empty dataframe with column names
#' This function takes a character vector and creates a 0-row dataframe with the vector as column name.
#' @param column_names vector of column names
#' @importFrom dplyr tibble
#' @importFrom dplyr slice
#' @export

blank_tibble <-
        function(column_names) {
                x <- rep("", length(column_names))
                names(x) <- column_names
                output <- vector_as_dataframe(vector = x) %>%
                                        dplyr::slice(-1)
                return(output)
        }
