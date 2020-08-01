#' Create a Blank Tibble
#' @description  This function takes a character vector and creates a 0-row dataframe with the vector as the column name names.  
#' @param column_names vector of column names
#' @importFrom dplyr slice
#' @importFrom purrr reduce
#' @export

blank_tibble <-
        function(column_names) {
                output <-
                column_names %>%
                        purrr::map(function(x) vector_to_tibble(vector = "",
                                                                new_col = !!x)) %>%
                        purrr::reduce(cbind) %>%
                        slice_first_row()
                        
                return(output)
        }
