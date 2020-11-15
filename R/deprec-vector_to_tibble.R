#' @title 
#' Convert a Vector to a Tibble 
#' @param vector        Vector that will become the column in the tibble
#' @param new_col       Name of the new column
#' @return
#' A tibble with 1 column with `new_col` as the name.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}}
#'  \code{\link[tibble]{tibble}}
#' @rdname vector_to_tibble
#' @export 
#' @importFrom dplyr enquo
#' @importFrom tibble tibble


vector_to_tibble <-
        function(vector, new_col) {
                .Deprecated(new = "as_tibble_col",
                            package = "tibble")
                new_col <- dplyr::enquo(new_col)
                tibble::tibble(!!new_col := vector)
        }