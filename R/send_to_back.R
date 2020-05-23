#' Send columns to back
#' @importFrom dplyr enquos
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @export

send_to_back <-
        function(dataframe, ...) {
                
                cols <- dplyr::enquos(...)
                
                
                cbind(
                        dataframe %>%
                                dplyr::select(-(!!!cols)),
                        dataframe %>%
                                dplyr::select(!!!cols)
                )
        }
