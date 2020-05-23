#' Rename all columns with a prefix
#' @importFrom dplyr rename_all
#' @export


rename_all_prefix <-
        function(dataframe,
                 prefix) {
                dataframe %>%
                        dplyr::rename_all(function(x) paste0(prefix, x))
        }