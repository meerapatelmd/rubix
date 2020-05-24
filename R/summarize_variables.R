#' Summarize a variable by the standard summary functions
#' @param ... grouping vars. If missing, all variables will be summarized.
#' @import tidyr 
#' @importFrom dplyr enquos
#' @importFrom dplyr summarize_all
#' @importFrom dplyr summarize_at
#' @importFrom dplyr mutate_all
#' @export

summarize_variables <-
        function(.data, ...) {
                
                summary_functions <-
                        list(
                                COUNT = ~ length(.),
                                DISTINCT_COUNT = ~ length(unique(.)),
                                NA_COUNT = ~ length(.[is.na(.)]),
                                NA_STR_COUNT = ~ length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]),
                                BLANK_COUNT = ~ length(.[. %in% c("")]),
                                DISTINCT_VALUES = ~ paste(unique(as.character(.)), collapse="|"),
                                DISTINCT_VALUES_EXPR = ~ cave::vector_to_string(.))

                if (!missing(...)) {
                        
                                cols <- dplyr::enquos(...)
                                
                                output_1 <-
                                        .data %>%
                                                dplyr::summarize_at(vars(!!!cols), 
                                                                    summary_functions)
                        
                } else {
                                output_1 <-
                                .data %>%
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
