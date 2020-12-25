#' Filter for the first row
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @export

filter_first_row <-
        function(.data, invert = FALSE) {
                if (invert) {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() != 1)
                        

                } else {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() == 1)
                        
                        
                }

        }