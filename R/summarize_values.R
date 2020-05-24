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
                                
                        output <-
                                cols <- enquos(...)
                                col_labels <- sapply(cols, rlang::as_name)
                                cols %>%
                                        purrr::map(function(x) .data %>%
                                                                group_by(!!x) %>%
                                                                summarize(COUNT = n())) %>%
                                        purrr::set_names(col_labels)
                        
                } else {
                        output <- 
                                cols <- colnames(.data)
                                #print(cols)
                                cols %>%
                                        purrr::map(function(x) .data %>%
                                                                dplyr::select(!!x) %>%
                                                           group_by_all() %>%
                                                           summarize(COUNT = n())) %>%
                                        purrr::set_names(cols)
                }
        }
