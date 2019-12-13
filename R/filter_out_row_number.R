#' Removes a row by its number
#' @param row.number integer
#' @importFrom dplyr filter
#' @export

rm_row_by_number <-
        function(dataframe, row.number) {
                dataframe %>%
                        dplyr::filter(row_number() != as.integer(row.number))
        }
