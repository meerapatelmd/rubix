#' Get the number of rows that differ between 2 dataframes
#' @importFrom dplyr setdiff
#' @export

nrow_diff <-
        function(x, y) {
                nrow(dplyr::setdiff(x, y))
        }
