#' @title
#' Rename all columns with a prefix
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname rename_all_prefix
#' @export 
#' @importFrom dplyr rename_all %>%


rename_all_prefix <-
        function(dataframe,
                 prefix) {
                dataframe %>%
                        dplyr::rename_all(function(x) paste0(prefix, x))
        }