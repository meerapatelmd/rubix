#' Summarize the counts of values in columns
#' @description To group by more than 1 variable can be summarized using summarize_grouped_vars().
#' @param ... grouping vars. If missing, groups and summarizes based on all columns.
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr enquos
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @export


summarize_values <-
        function(.data,
                 ...,
                 desc = TRUE) {
                
                if (!missing(...)) {
                                
                                cols <- enquos(...)
                                col_labels <- sapply(cols, rlang::as_name)
                                cols %>%
                                        purrr::map(function(x) .data %>% 
                                                                dplyr::select(!!x) %>% 
                                                                dplyr::rename_all(stringr::str_replace_all, "^.*$", "Value") %>% 
                                                                dplyr::group_by_all() %>%
                                                                summarize(COUNT = n())) %>%
                                        purrr::set_names(col_labels) %>%
                                        dplyr::bind_rows(.id = "Variable")
                        
                } else {
                        
                                cols <- colnames(.data)
                                #print(cols)
                                cols %>%
                                        purrr::map(function(x) .data %>%
                                                                dplyr::select(!!x) %>% 
                                                                dplyr::rename_all(stringr::str_replace_all, "^.*$", "Value") %>%
                                                           dplyr::group_by_all() %>%
                                                           dplyr::summarize(COUNT = n())) %>%
                                        purrr::set_names(cols) %>%
                                        dplyr::bind_rows(.id = "Variable")
                }
                

        }
