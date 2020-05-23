#' Replace a pattern in all column names
#' @importFrom dplyr rename_all
#' @importFrom stringr str_remove_all
#' @export

rename_all_remove <-
        function(dataframe,
                 pattern) {
                dataframe %>%
                        dplyr::rename_all(function(x) stringr::str_remove_all(x, 
                                                                               pattern = pattern))
        }