#' Mutate a column to position 1
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export


mutate1 <-
        function(dataframe, column_name, value) {
                column_name <- dplyr::enquo(column_name)
                dataframe %>%
                        dplyr::mutate(`:=`(!!column_name, value)) %>%
                        dplyr::select(!!column_name, everything())
        }

