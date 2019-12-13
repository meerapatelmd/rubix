#' Takes a character vector and creates a 0-row dataframe with the vectors as column names
#' @importFrom dplyr tibble
#' @export

blank_tibble <-
        function(column_names) {
                x <- dplyr::tibble(matrix(ncol = length(column_names)), stringsAsFactors = FALSE)
                colnames(x) <- column_names
                x <- x[-1,]
                return(x)
        }
