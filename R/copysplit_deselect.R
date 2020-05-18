#' Split a dataframe into a list with the column removed
#' @importFrom dplyr select
#' @export


split_deselect <-
        function(x, column_name, ...) {
                output <- split(x, x[,column_name] %>% unlist(), ...)
                output %>%
                        map_names_set(function(y) y  %>%
                                                        dplyr::select(-all_of(column_name)))
        }

