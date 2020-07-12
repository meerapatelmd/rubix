#' Filter for a row n
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @export

filter_row_n <-
        function(.data, n, invert = FALSE) {
                if (invert) {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() != n)
                        

                } else {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() == n)
                        
                        
                }

        }