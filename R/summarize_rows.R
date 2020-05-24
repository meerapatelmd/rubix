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
