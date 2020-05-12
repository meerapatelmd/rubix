#' Get count and distinct count of a column
#' @return A dataframe with columns "Variable" for input column name, "Desc" for description of the count type, and "Count" for the value.
#' @importFrom dplyr enquo
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export

pivot_count <-
        function(dataframe, col) {
                column_name <- deparse(substitute(col))
                col <- dplyr::enquo(col)
                
                col_vector <-
                        dataframe %>%
                        dplyr::select(!!col) %>%
                        unlist()
                
                output <- list()
                output[[1]] <-
                        tibble::tibble(
                                Desc = c("Total Count", "Distinct Count"),
                                Counts = c(length(col_vector),
                                           length(unique(col_vector)))) 
                
                names(output)[1] <- column_name
                
                output <- dplyr::bind_rows(output, .id = "Variable")
                return(output)
                
        }