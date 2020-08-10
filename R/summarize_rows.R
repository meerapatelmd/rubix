#' Summarize the counts of values in columns
#' @description To group by more than 1 variable can be summarized using summarize_grouped_vars().
#' @param ... grouping vars. If missing, groups and summarizes based on all columns.
#' @seealso 
#'  \code{\link[cave]{vector_to_string}}
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}},\code{\link[dplyr]{group_by_all}}
#' @rdname summarize_rows
#' @export 
#' @importFrom cave vector_to_string
#' @importFrom dplyr enquos group_by summarize arrange ungroup desc group_by_at group_by_all %>% 

summarize_rows <-
        function(dataframe,
                 ...,
                 desc = TRUE) {
                
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
                                
                                if (desc == FALSE) {
                                        dataframe %>%
                                                dplyr::group_by(!!!cols) %>%
                                                dplyr::summarize(COUNT = n()) %>%
                                                dplyr::arrange(COUNT) %>%
                                                dplyr::ungroup()
                                } else {
                                        dataframe %>%
                                                dplyr::group_by(!!!cols) %>%
                                                dplyr::summarize(COUNT = n()) %>%
                                                dplyr::arrange(dplyr::desc(COUNT)) %>%
                                                dplyr::ungroup()
                                }
                        
                } else {
                        
                        if (desc == FALSE) {
                                dataframe %>% 
                                        dplyr::group_by_at(vars(everything())) %>%
                                        dplyr::summarize(COUNT = n()) %>%
                                        dplyr::arrange(COUNT) %>%
                                        dplyr::ungroup()
                                
                        } else {
                                dataframe %>%
                                        dplyr::group_by_all() %>%
                                        dplyr::summarize(COUNT = n()) %>%
                                        dplyr::arrange(dplyr::desc(COUNT)) %>%
                                        dplyr::ungroup()
                        }
                        
                }
                
                
        }
