#' Split a dataframe into a list with the column removed
#' @importFrom dplyr select
#' @export


split_by <-
        function(x, column) {
                column <- enquo(column)
                split(x, x %>%
                              dplyr::select(!!column) %>%
                              unlist() %>%
                              unique())
        }

