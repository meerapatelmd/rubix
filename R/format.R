#' @title 
#' Format Column Names
#' @description 
#' Format is defined as 
#' 1) all letters converted to upper, 
#' 2) trailing or leading whitespace removed, 
#' 3) all punctuation or spaces of any length replaced with a single underscore, 
#' 4) any trailing underscores are removed, which represent terminal punctuation in the column name
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname format_colnames
#' @export 
#' @importFrom dplyr rename_all %>%

format_colnames <-
        function(.data) {
                x <-
                        .data %>%
                                dplyr::rename_all(trimws, "both") %>%
                                dplyr::rename_all(toupper) %>%
                                dplyr::rename_all(str_replace_all, "[[:punct:]]{1,}|[ ]{1,}", "_") %>%
                                dplyr::rename_all(str_remove_all, "[_]{1}$")

                return(x)
        }





#' @title
#' Standardize List Names
#' @description
#' This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
#' @seealso 
#'  \code{\link[purrr]{map}}
#'  \code{\link[centipede]{trimws}}
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#' @rdname format_names
#' @export
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom centipede trimws
#' @importFrom stringr str_replace_all str_remove_all

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





