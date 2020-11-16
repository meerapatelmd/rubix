#' Rename all columns with a prefix
#' @importFrom dplyr rename_at
#' @export


rename_at_prefix <-
        function(data,
                 ...,
                 prefix) {
                
                cols <- dplyr::enquos(...)
                
                data %>%
                        dplyr::rename_at(vars(!!!cols), function(x) paste0(prefix, x))
        }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_at
#' @importFrom stringr str_remove_all
#' @export

rename_at_remove <-
        function(data,
                 ...,
                 pattern) {
                
                cols <- dplyr::enquos(cols)
                
                data %>%
                        dplyr::rename_at(vars(!!!cols), function(x) stringr::str_remove_all(x, 
                                                                               pattern = pattern))
        }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_at
#' @importFrom stringr str_remove_all
#' @export

rename_at_replace <-
        function(data,
                 ...,
                 pattern,
                 replacement) {
                
                cols <- dplyr::enquos(cols)
                
                data %>%
                        dplyr::rename_at(vars(!!!cols), function(x) stringr::str_replace_all(x, 
                                                                               pattern = pattern,
                                                                               replacement = replacement))
        }





#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_at_suffix <-
        function(data,
                 ...,
                 suffix) {
                
                cols <- enquos(...)
                
                data %>%
                        dplyr::rename_at(vars(!!!cols), function(x) paste0(x, suffix))
        }












