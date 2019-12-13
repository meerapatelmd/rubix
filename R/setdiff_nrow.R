#' Get the number of rows that differ between 2 dataframes
#' @importFrom dplyr setdiff
#' @export

setdiff_nrow <-
        function(x, y) {
                nrow(dplyr::setdiff(x, y))
        }
