#' Bring Columns to the Front
#' @description Bring a vector of columns to the front of a dataframe. 
#' @importFrom magrittr %>% 
#' @importFrom dplyr enquos
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @export


bring_to_front <-
        function(.data, ...) {
                cols <- dplyr::enquos(...)
                .data %>% 
                        dplyr::select(!!!cols, dplyr::everything())
        }

