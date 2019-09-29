#' Left joins dataframes in the same named list
#' @param list list of dataframes of length of 2 or greater
#' @param x_name name of the dataframe x
#' @param y_name name of dataframe y
#' @importFrom dplyr left_join
#' @export

left_join_with_list_names <-
        function(list, x_name, y_name, by = NULL, suffix = c(".x", ".y"), ...) {
                x <- list[[x_name]]
                y <- list[[y_name]]
                
                x %>%
                        dplyr::left_join(y, by = by, suffix = suffix, ...)
        }