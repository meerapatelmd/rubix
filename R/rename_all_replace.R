#' Replace a pattern in all column names
#' @importFrom dplyr rename_all
#' @importFrom stringr str_replace_all
#' @export

rename_all_replace <-
        function(dataframe,
                 pattern,
                 replacement) {
                dataframe %>%
                        dplyr::rename_all(function(x) stringr::str_replace_all(x, 
                                                                               pattern = pattern,
                                                                               replacement = replacement))
        }