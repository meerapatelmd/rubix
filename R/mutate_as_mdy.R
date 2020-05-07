#' Mutate given cols into a mdy date
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate_at
#' @importFrom lubridate mdy
#' @export

mutate_as_mdy <-
        function(dataframe,
                 ...) {
                
                Args <- dplyr::enquos(...)
                dataframe %>%
                        dplyr::mutate_at(vars(!!!Args), lubridate::mdy)
        }