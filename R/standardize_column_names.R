#' Standardize column names
#' @description This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
#' @importFrom  dplyr rename_all
#' @export


standardize_column_names <-
        function(dataframe) {
                        dataframe %>%
                                dplyr::rename_all(trimws, "both") %>%
                                dplyr::rename_all(toupper) %>%
                                dplyr::rename_all(str_replace_all, "[[:punct:]]{1,}", "_") %>%
                                dplyr::rename_all(str_remove_all, "[_]{1}$") %>%
                                dplyr::rename_all(str_replace_all, " ", "_")
        }
