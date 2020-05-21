#' Take a single vector of values and convert it to a dataframe
#' @description This function was written in cases were a list of vectors needed to be aggregated either by bind_cols and bind_rows of the dplyr package.
#' @importFrom tibble tibble
#' @importFrom dplyr enquo
#' @export


vector_to_tibble <-
        function(vector, new_col) {
                new_col <- dplyr::enquo(new_col)
                tibble::tibble(!!new_col := vector)
        }