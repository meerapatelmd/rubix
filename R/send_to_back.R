#' Send columns to back
#' @importFrom dplyr enquos
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @export

send_to_back <-
        function(.data, ...) {
                
                cols <- dplyr::enquos(...)
                
                .data_back <-
                .data %>% 
                        dplyr::select(!!!cols)
                
                .data_front <- 
                        .data %>% 
                        dplyr::select(-all_of(colnames(.data_back)))
                
                .data_front %>%
                        dplyr::bind_cols(.data_back)
        }
