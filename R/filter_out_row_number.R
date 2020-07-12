#' Removes a row by its number
#' @param row.number integer
#' @importFrom dplyr filter
#' @export

filter_out_row_number <-
        function(dataframe, row.number) {
                dataframe %>%
                        dplyr::filter(row_number() != as.integer(row.number))
        }
