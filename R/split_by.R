#' Split a dataframe into a list with the column removed
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export


split_by <-
        function(x, column) {
                
                column <- dplyr::enquo(column)
                
                split(x, 
                      x %>%
                              dplyr::select(!!column) %>%
                              unlist() %>%
                              unique())
        }

