#' Bring columns to front
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

