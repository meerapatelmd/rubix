#' Mutate given columns to ymd date
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate_at
#' @importFrom lubridate ymd
#' @export

mutate_as_ymd <-
        function(dataframe,
                 ...) {
                
                Args <- dplyr::enquos(...)
                dataframe %>%
                        dplyr::mutate_at(vars(!!!Args), lubridate::ymd)
        }
