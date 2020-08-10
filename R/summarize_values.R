#' Summarize the counts of values in columns
#' @description To group by more than 1 variable can be summarized using summarize_grouped_vars().
#' @param ... grouping vars. If missing, groups and summarizes based on all columns.
#' @seealso 
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{summarise}}
#'  \code{\link[stringr]{str_replace}}
#' @rdname summarize_values
#' @export 
#' @importFrom rlang as_name
#' @importFrom purrr map set_names
#' @importFrom dplyr select rename_all group_by_all bind_rows summarize %>% 
#' @importFrom stringr str_replace_all


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
