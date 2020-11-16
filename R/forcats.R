#' @export

categorize <-
        function(data, col, ...) {

                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_collapse, ...) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), as.character)
                
        }

#' @export

categorize_ff <-
        function(...) {
                
                
                function(data,
                         col) {
                        
                
                
                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_collapse, ...) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), as.character)
                
                }
        }

#' @export

recode_value <- 
        function(data, col, ...) {
                
                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_recode, ...) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), as.character)
                
        }

#' @export

recode_boolean <- 
        function(data, col, true_value, false_value) {
                
                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_collapse, `TRUE` = true_value, `FALSE` = false_value) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), as.logical)
                
        }




