#' Split a dataframe into a list with the column removed
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export


split_deselect <-
        function(x, column) {
                
                column <- dplyr::enquo(column)
                
                output <- 
                        split_by(x = x,
                                 column = !!column)
                
                output %>%
                        map_names_set(function(y) y  %>%
                                                        dplyr::select(-(!!column)))
                
                
        }

