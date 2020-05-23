#' Bring columns to front
#' @importFrom dplyr enquos
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @export


bring_to_front <-
        function(dataframe, ...) {
                cols <- dplyr::enquos(...)
                dataframe %>% 
                        dplyr::select(!!!cols, dplyr::everything())
        }

