#' @title 
#' Summarize groups by count
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}}
#' @rdname summarize_grouped_n
#' @export 
#' @importFrom dplyr enquos group_by summarize arrange ungroup desc %>%


summarize_grouped_n <-
        function(dataframe,
                 ...,
                 desc = FALSE) {
                
                cols <- dplyr::enquos(...)
                
                if (desc == FALSE) {
                        dataframe %>%
                                dplyr::group_by(!!!cols) %>%
                                dplyr::summarize(n = n()) %>%
                                dplyr::arrange(n) %>%
                                dplyr::ungroup()
                } else {
                        dataframe %>%
                                dplyr::group_by(!!!cols) %>%
                                dplyr::summarize(n = n()) %>%
                                dplyr::arrange(dplyr::desc(n)) %>%
                                dplyr::ungroup()
                }
                
        }





#' Summarizes each column with max value
#' @param ... column names for vectors of any data class
#' @param na.rm TRUE if true NA are to be removed. Default is TRUE.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}}
#' @rdname summarize_max
#' @export 
#' @importFrom dplyr enquos %>%

summarize_max <-
        function(.data, ..., na.rm = TRUE) {
                max_value_vars <- dplyr::enquos(...)

                return(
                        .data %>%
                                mutate_at(vars(!!!max_value_vars), funs(as.double(as.character(.)))) %>%
                                summarise_at(vars(!!!max_value_vars), max, na.rm = na.rm)
                )
        }





#' @title
#' Summarize a variable by the standard summary functions
#' @param ... grouping vars. If missing, all variables will be summarized.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{mutate_all}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{pivot_wider}}
#' @rdname summarize_numeric_vars
#' @export 
#' @importFrom dplyr enquos select summarize_all select_if mutate_all %>%
#' @importFrom tidyr pivot_longer pivot_wider

summarize_numeric_vars <-
        function(.data, ...) {
                
                is_integer_or_number <-
                        function(x) {
                                if (is.numeric(x)) {
                                        return(TRUE)
                                } else {
                                        if (is.integer(x)) {
                                                return(TRUE)
                                        } else {
                                                if (is.double(x)) {
                                                        return(TRUE)
                                                } else {
                                                        return(FALSE)
                                                }
                                        }
                                }
                        }
                
                summary_functions <-
                        list(
                                MEAN = ~ mean(., na.rm = TRUE),
                                MEAN_NA = ~ mean(., na.rm = FALSE),
                                MEDIAN = ~ median(., na.rm = TRUE),
                                MEDIAN_NA = ~ median(., na.rm = FALSE),
                                SD = ~ sd(., na.rm = TRUE),
                                SD_NA = ~ sd(., na.rm = FALSE),
                                MAX = ~ max(., na.rm = TRUE),
                                MAX_NA = ~ max(., na.rm = FALSE),
                                MIN = function(x) min(x, na.rm = TRUE),
                                MIN_NA = function(x) min(x, na.rm = FALSE))

                if (!missing(...)) {
                        
                                cols <- dplyr::enquos(...)
                                
                        
                                .data_qa <-
                                    .data %>%
                                            dplyr::select(!!!cols)
                                
                                
                                
                                .data_qa <- sapply(.data_qa, is_integer_or_number, USE.NAMES = TRUE)
                                .data_qa <- .data_qa[.data_qa == FALSE]
                                
                                if (length(.data_qa) > 0) {
                                        stop(paste(names(.data_qa), collapse = ", "), ": not numeric, integer, or double.")
                                }
                               
                                
                                    
                                output_1 <-
                                        .data %>%
                                                dplyr::select(!!!cols) %>%
                                                dplyr::summarize_all(summary_functions)
                        
                } else {
                                output_1 <-
                                .data %>%
                                        dplyr::select_if(is_integer_or_number) %>%
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
#' @importFrom centipede no_na
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
                                                DISTINCT_STR = ~paste(sort(unique(as.character(.))), collapse = "|"),
                                                DISTINCT_STR_NA = ~paste(sort(unique(as.character(.)) %>% centipede::no_na()), collapse = "|")
                                        ),
                                        categorical = list(
                                                COUNT = ~length(.), 
                                                DISTINCT_COUNT = ~length(unique(.)), 
                                                NA_COUNT = ~length(.[is.na(.)]), 
                                                NA_STR_COUNT = ~length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]), 
                                                BLANK_COUNT = ~length(.[. %in%  c("")]), 
                                                DISTINCT_VALUES = ~paste(sort(unique(as.character(.))), collapse = "|"),
                                                DISTINCT_VALUES_NA = ~paste(sort(unique(as.character(.)) %>% centipede::no_na()), collapse = "|")
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
                
                
                if (incl_expr) {
                
                summary_functions <-
                        list(
                                COUNT = ~ length(.),
                                DISTINCT_COUNT = ~ length(unique(.)),
                                NA_COUNT = ~ length(.[is.na(.)]),
                                NA_STR_COUNT = ~ length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]),
                                BLANK_COUNT = ~ length(.[. %in% c("")]),
                                DISTINCT_VALUES = ~ paste(unique(as.character(.)), collapse="|"),
                                DISTINCT_VALUES_EXPR = ~ cave::vector_to_string(unique(.)))
                } else {
                        summary_functions <-
                                list(
                                        COUNT = ~ length(.),
                                        DISTINCT_COUNT = ~ length(unique(.)),
                                        NA_COUNT = ~ length(.[is.na(.)]),
                                        NA_STR_COUNT = ~ length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]),
                                        BLANK_COUNT = ~ length(.[. %in% c("")]),
                                        DISTINCT_VALUES = ~ paste(unique(as.character(.)), collapse="|"))
                }

                output_1 <-
                .data %>%
                        dplyr::summarize_all(summary_functions)
                                
                        
                
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





