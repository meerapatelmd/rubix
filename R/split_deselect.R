#' Split a dataframe into a list with the column removed
#' @importFrom dplyr select
#' @export


split_deselect <-
        function(x, column) {
                
                column <- enquo(column)
                output <- 
                        split_by(x = x,
                                 column = !!column)
                
                output %>%
                        map_names_set(function(y) y  %>%
                                                        dplyr::select(-(!!column)))
                
                
        }

