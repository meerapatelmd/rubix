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
#' @importFrom rlang as_name
#' @importFrom dplyr mutate_at group_by_at summarise_at mutate rename select everything
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%

summarize_values <-
        function(.data,
                 ...,
                 names_to = "name",
                 values_to = "value") {
                                summaryFunLibrary <- 
                                        list(numerical = list(
                                                MEAN = ~mean(., na.rm = TRUE), 
                                                MEAN_NA = ~mean(., na.rm = FALSE), 
                                                MEDIAN = ~median(., na.rm = TRUE), 
                                                MEDIAN_NA = ~median(., na.rm = FALSE), 
                                                SD = ~sd(., na.rm = TRUE), 
                                                SD_NA = ~sd(., na.rm = FALSE), 
                                                MAX = ~max(., na.rm = TRUE), 
                                                MAX_NA = ~max(., na.rm = FALSE), 
                                                MIN = function(x) min(x, na.rm = TRUE), 
                                                MIN_NA = function(x) min(x, na.rm = FALSE),
                                                SUM = ~sum(.,na.rm = TRUE),
                                                SUM_NA = ~sum(.,na.rm = FALSE),
                                                DISTINCT_LENGTH = ~length(unique(.)),
                                                NA_LENGTH = ~length(.[is.na(.)]), 
                                                BLANK_LENGTH = ~length(.[. %in%  c("")]), 
                                                DISTINCT_VALUES_AS_CHAR = ~paste(sort(unique(as.character(.))), collapse = "|")
                                        ),
                                        categorical = list(
                                                COUNT = ~length(.), 
                                                DISTINCT_COUNT = ~length(unique(.)), 
                                                NA_COUNT = ~length(.[is.na(.)]), 
                                                NA_STR_COUNT = ~length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]), 
                                                BLANK_COUNT = ~length(.[. %in%  c("")]), 
                                                DISTINCT_VALUES = ~paste(unique(as.character(.)), collapse = "|")
                                        )
                                        )
                                
                                cols <- enquos(...)
                                col_labels <- sapply(cols, rlang::as_name) 
                                
                                inverse_col_labels <- colnames(.data)[!(colnames(.data) %in% col_labels)]
                                #print(inverse_col_labels)
                                
                                # .data %>% 
                                #         dplyr::group_by_at(vars(!!!cols)) %>%
                                #         dplyr::summarize_at(vars(!!summary_var),
                                #                             ~length(.)) %>%
                                #         dplyr::ungroup()
                                #         
                                
                                .data %>%
                                        dplyr::mutate_at(vars(all_of(inverse_col_labels)), as.character) %>% 
                                        tidyr::pivot_longer(cols = all_of(inverse_col_labels),
                                                            names_to = names_to,
                                                            values_to = values_to) %>%
                                        dplyr::group_by_at(vars(c(!!!cols,
                                                                  !!names_to))) %>%
                                        dplyr::summarise_at(vars(!!values_to),
                                                           summaryFunLibrary$categorical) %>%
                                        dplyr::mutate(NET_COUNT = COUNT-(NA_COUNT+NA_STR_COUNT+BLANK_COUNT)) %>%
                                        dplyr::rename(!!values_to := DISTINCT_VALUES) %>%
                                        dplyr::select(!!!cols,
                                                      !!names_to,
                                                      !!values_to,
                                                      NET_COUNT,
                                                      dplyr::everything())
                                
                

        }
