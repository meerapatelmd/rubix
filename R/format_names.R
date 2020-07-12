#' Standardize List Names
#' This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
#' @importFrom purrr map
#' @importFrom centipede trimws
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @export

format_names <-
        function(list) {
                names(list) <-
                        names(list) %>%
                                purrr::map(centipede::trimws, "both") %>%
                                purrr::map(toupper) %>%
                                purrr::map(stringr::str_replace_all, "[[:punct:]]{1,}|[ ]{1,}", "_") %>%
                                purrr::map(stringr::str_remove_all, "[_]{1}$")
                return(list)
        }
