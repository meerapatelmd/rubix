#' Replace a pattern in all column names
#' @importFrom dplyr rename_at
#' @importFrom stringr str_remove_all
#' @export

rename_at_replace <-
        function(dataframe,
                 ...,
                 pattern,
                 replacement) {
                
                cols <- dplyr::enquos(cols)
                
                dataframe %>%
                        dplyr::rename_at(vars(!!!cols), function(x) stringr::str_replace_all(x, 
                                                                               pattern = pattern,
                                                                               replacement = replacement))
        }