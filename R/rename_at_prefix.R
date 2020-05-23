#' Rename all columns with a prefix
#' @importFrom dplyr rename_at
#' @export


rename_at_prefix <-
        function(dataframe,
                 ...,
                 prefix) {
                
                cols <- dplyr::enquos(...)
                
                dataframe %>%
                        dplyr::rename_at(vars(!!!cols), function(x) paste0(prefix, x))
        }