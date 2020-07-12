#' Format Column Names
#' @description Format is defined as 
#' 1) all letters converted to upper, 
#' 2) trailing or leading whitespace removed, 
#' 3) all punctuation or spaces of any length replaced with a single underscore, 
#' 4) any trailing underscores are removed, which represent terminal punctuation in the column name
#' @importFrom  dplyr rename_all
#' @export

format_colnames <-
        function(dataframe) {
                x <-
                        dataframe %>%
                                dplyr::rename_all(trimws, "both") %>%
                                dplyr::rename_all(toupper) %>%
                                dplyr::rename_all(str_replace_all, "[[:punct:]]{1,}|[ ]{1,}", "_") %>%
                                dplyr::rename_all(str_remove_all, "[_]{1}$")

                return(x)
        }
