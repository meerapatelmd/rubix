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





#' Replace a pattern in all column names
#' @importFrom dplyr rename_all
#' @importFrom stringr str_remove_all
#' @export

rename_all_remove <-
        function(dataframe,
                 pattern) {
                dataframe %>%
                        dplyr::rename_all(function(x) stringr::str_remove_all(x, 
                                                                               pattern = pattern))
        }





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





#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_all_suffix <-
        function(dataframe,
                 suffix) {
                dataframe %>%
                        dplyr::rename_all(function(x) paste0(x, suffix))
        }












#' Rename all columns with a prefix
#' @importFrom dplyr rename_all
#' @export


rename_all_with_prefix <-
        function(dataframe,
                 prefix) {
                dataframe %>%
                        dplyr::rename_all(function(x) paste0(prefix, x))
        }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_all
#' @importFrom stringr str_replace_all
#' @export

rename_all_with_replace <-
        function(dataframe,
                 pattern,
                 replacement) {
                dataframe %>%
                        dplyr::rename_all(function(x) stringr::str_replace_all(x, 
                                                                               pattern = pattern,
                                                                               replacement = replacement))
        }





#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_all_with_suffix <-
        function(dataframe,
                 suffix) {
                dataframe %>%
                        dplyr::rename_all(function(x) paste0(x, suffix))
        }












