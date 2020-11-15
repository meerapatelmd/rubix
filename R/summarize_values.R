#' Summarize the counts of values in columns
#' @description To group by more than 1 variable can be summarized using summarize_grouped_vars().
#' @param .data                 data frame      
#' @param ...                   group by variables     
#' @param names_to              passed to tidyr pivot_longer()
#' @param values_to             passed to tidyr pivot_longer()
#' @seealso 
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{summarise}}
#'  \code{\link[stringr]{str_replace}}
#' @seealso 
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{rename}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#'  \code{\link[tidyr]{pivot_longer}}
#' @rdname summarize_values
#' @export 
#' @export 
#' @importFrom rlang as_name
#' @importFrom dplyr mutate_at group_by_at summarize
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%

summarize_values <-
        function(.data,
                 ...,
                 names_to = "VARIABLE",
                 values_to = "VALUE") {
                
                if (missing(...)) {
                        stop('grouping vars required')
                }
                
                                load_sum_fun_library()
                                
                                cols <- enquos(...)
                                col_labels <- sapply(cols, rlang::as_name) 
                                
                                inverse_col_labels <- colnames(.data)[!(colnames(.data) %in% col_labels)]
                                
                                data1 <-
                                        .data %>% 
                                                dplyr::mutate_at(vars(all_of(inverse_col_labels)), as.character) %>%
                                                tidyr::pivot_longer(cols = inverse_col_labels,
                                                                    names_to = names_to,
                                                                    values_to = values_to,
                                                                    values_drop_na = FALSE)  
                                
                                
                                group_data <- 
                                        data1 %>%
                                        dplyr::filter_at(vars(!!values_to), ~!is.na(.)) %>%
                                        dplyr::group_by_at(vars(c(!!!cols, 
                                                                !!names_to,
                                                                !!values_to))) %>%
                                                dplyr::summarize(COUNT = n(),
                                                                 .groups = "drop")
                                
                                
                                data2 <- 
                                        data1 %>%
                                        dplyr::select(!!names_to,
                                                      !!values_to) %>%
                                        dplyr::distinct()

                                
                                dplyr::full_join(data2,
                                                 group_data,
                                                 by = c(names_to,
                                                        values_to)) %>%
                                        dplyr::mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT))

        }
