#' Standardize Column Names
#' This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
#' @inheritParams call_mr_clean
#' @importFrom  dplyr rename_all
#' @export

cleanup_colnames <-
        function(dataframe) {
                .Deprecated()
                x <-
                        dataframe %>%
                                dplyr::rename_all(trimws, "both") %>%
                                dplyr::rename_all(toupper) %>%
                                dplyr::rename_all(str_replace_all, "[[:punct:]]{1,}", "_") %>%
                                dplyr::rename_all(str_remove_all, "[_]{1}$") %>%
                                dplyr::rename_all(str_replace_all, " {1,}", "_")
                colnames(x) <- gsub("[_]{2,}", "_", colnames(x))

                return(x)
        }
