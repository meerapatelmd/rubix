#' Summarize a variable by the standard summary functions
#' @param ... grouping vars. If missing, all variables will be summarized.
#' @import tidyr 
#' @importFrom dplyr enquos
#' @importFrom dplyr summarize_all
#' @importFrom dplyr summarize_at
#' @importFrom dplyr mutate_all
#' @export

summarize_numeric_vars <-
        function(.data, ...) {
                
                is_integer_or_number <-
                        function(x) {
                                if (is.numeric(x)) {
                                        return(TRUE)
                                } else {
                                        if (is.integer(x)) {
                                                return(TRUE)
                                        } else {
                                                if (is.double(x)) {
                                                        return(TRUE)
                                                } else {
                                                        return(FALSE)
                                                }
                                        }
                                }
                        }
                
                summary_functions <-
                        list(
                                MEAN = ~ mean(., na.rm = TRUE),
                                MEAN_NA = ~ mean(., na.rm = FALSE),
                                MEDIAN = ~ median(., na.rm = TRUE),
                                MEDIAN_NA = ~ median(., na.rm = FALSE),
                                SD = ~ sd(., na.rm = TRUE),
                                SD_NA = ~ sd(., na.rm = FALSE),
                                MAX = ~ max(., na.rm = TRUE),
                                MAX_NA = ~ max(., na.rm = FALSE),
                                MIN = function(x) min(x, na.rm = TRUE),
                                MIN_NA = function(x) min(x, na.rm = FALSE))

                if (!missing(...)) {
                        
                                cols <- dplyr::enquos(...)
                                
                                output_1 <-
                                        .data %>%
                                                dplyr::select(!!!cols) %>%
                                                dplyr::summarize_all(summary_functions)
                        
                } else {
                                output_1 <-
                                .data %>%
                                        dplyr::select_if(is_integer_or_number) %>%
                                        dplyr::summarize_all(summary_functions)
                                
                        
                }
                
                output_2 <- 
                        output_1   %>%
                        dplyr::mutate_all(as.character) %>%
                        tidyr::pivot_longer(cols = everything(),
                                            names_pattern = paste0("(^.*?)[_](", paste(paste0(names(summary_functions), "$"), collapse = "|"), ")"),
                                            names_to = c("Variable", "Parameter"),
                                            values_to = "Value") 
                
                output_3 <- 
                        output_2 %>%
                        tidyr::pivot_wider(id_cols = Variable,
                                           names_from = Parameter,
                                           values_from = Value) 
                
                                
                
                return(output_3)
        }
