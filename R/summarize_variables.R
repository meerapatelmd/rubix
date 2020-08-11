#' Summarize a Variable
#' @param ... grouping vars. If missing, all variables will be summarized.
#' @seealso 
#'  \code{\link[cave]{vector_to_string}}
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate_all}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{pivot_wider}}
#' @rdname summarize_variables
#' @export 
#' @importFrom cave vector_to_string
#' @importFrom dplyr enquos summarize_at summarize_all mutate_all %>% 
#' @importFrom tidyr pivot_longer pivot_wider

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
                                DISTINCT_VALUES_EXPR = ~ cave::vector_to_string(unique(.)))

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
