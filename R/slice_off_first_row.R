#' Slice first row
#' @importFrom dplyr slice
#' @export

slice_off_first_row <-
        function(dataframe) {
                dataframe %>%
                        dplyr::slice(-1)
        }