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
        function(.data, 
                 ...,
                 names_to = "VARIABLE") {
                
                
                        load_sum_fun_library()
                        
                        cols <- enquos(...)
                        col_labels <- sapply(cols, rlang::as_name) 
                        
                        inverse_col_labels <- colnames(.data)[!(colnames(.data) %in% col_labels)]
                        
                        .data %>%
                                dplyr::mutate_at(vars(all_of(inverse_col_labels)), as.character) %>% 
                                tidyr::pivot_longer(cols = all_of(inverse_col_labels),
                                                    names_to = names_to,
                                                    values_drop_na = TRUE)  %>%
                                dplyr::group_by_at(vars(c(!!!cols,
                                                          !!names_to))) %>%
                                dplyr::summarise_at(vars(all_of("value")),
                                                    summaryFunLibrary$categorical)
        }
