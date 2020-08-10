#' Summarize a variable by the standard summary functions
#' @param ... grouping vars. If missing, all variables will be summarized.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate_all}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{pivot_wider}}
#' @rdname summarize_var_group
#' @export 
#' @importFrom dplyr enquos group_by_at summarize_all mutate_at %>%
#' @importFrom tidyr pivot_longer pivot_wider

summarize_var_group <-
        function(.data, ...) {
                
                summary_functions <-
                        list(
                                COUNT = ~ length(.),
                                DISTINCT_COUNT = ~ length(unique(.)),
                                NA_COUNT = ~ length(.[is.na(.)]),
                                NA_STR_COUNT = ~ length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]),
                                BLANK_COUNT = ~ length(.[. %in% c("")]),
                                DISTINCT_VALUES = ~ paste(unique(as.character(.)), collapse="|") #,
                                #DISTINCT_VALUES_EXPR = ~ cave::vector_to_string(.)
                                )

                if (!missing(...)) {
                        
                                cols <- dplyr::enquos(...)
                                
                                output_1 <-
                                        .data %>% 
                                                dplyr::group_by_at(vars(!!!cols)) %>%
                                                dplyr::summarize_all(summary_functions) %>%
                                                bring_to_front(!!!cols)
                
                output_2 <- 
                        output_1   %>%
                        dplyr::mutate_at(vars(-group_cols()), as.character) %>%
                         tidyr::pivot_longer(col = contains(names(summary_functions)),
                                             names_pattern = paste0("(^.*?)[_](", paste(paste0(names(summary_functions), "$"), collapse = "|"), ")"),
                                             names_to = c("Variable", "Parameter"),
                                             values_to = "Value")
                
                output_3 <- 
                        output_2  %>%
                         tidyr::pivot_wider(id_cols = c(!!!cols, Variable),
                                           names_from = Parameter,
                                            values_from = Value) 
                
                                
                
                return(output_3)
                
                } else {
                        stop("group_cols() not provided. Use summarize_variables() for ungrouped summary.")
                }
        }
