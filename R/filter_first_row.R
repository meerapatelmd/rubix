#' Filter for the first row
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @export

filter_first_row <-
        function(dataframe) {
                dataframe %>%
                        dplyr::filter(dplyr::row_number() == 1)
        }