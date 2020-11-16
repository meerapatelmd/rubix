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
                
                .Deprecated(new = "count",
                            package = "dplyr")
                
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
#' @param data A dataframe or tibble. 
#' @param ... column names for vectors of any data class
#' @param na.rm TRUE if true NA are to be removed. 
#' @param grouper (optional) Group by column.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}}
#' @rdname summarize_max
#' @example inst/examples/summarize_max.R
#' @family summarize functions
#' @export 
#' @importFrom dplyr enquos select summarise_at vars all_of group_by_at ungroup group_by

summarize_max <-
        function(data, 
                 ..., 
                 na.rm = TRUE,
                 grouper) {
                
                if (missing(grouper)) {
                        
                        if (!missing(...)) {
                                
                                all_num_cols <- all_numeric_cols(data = data)
                                
                                cols <- dplyr::enquos(...)
                                selected_cols <- 
                                        data %>%
                                                dplyr::select(!!!cols) %>%
                                                colnames()
                                
                                max_value_vars <- selected_cols[selected_cols %in% all_num_cols]
                                
                                if (length(max_value_vars) == 0) {
                                        
                                        stop("columns are not numeric")
                                        
                                }
                        
                        } else {
                                
                                max_value_vars <- all_numeric_cols(data = data)
                                
                        }
                        
                        data %>%
                                dplyr::summarise_at(dplyr::vars(dplyr::all_of(max_value_vars)), max, na.rm = na.rm)
                        
                } else {
                        
                        if (!missing(...)) {

                                max_value_vars <- dplyr::enquos(...)

                                data %>%
                                        dplyr::group_by_at(dplyr::vars({{ grouper }})) %>%
                                        dplyr::summarise_at(dplyr::vars(!!!max_value_vars), max, na.rm = na.rm) %>%
                                        dplyr::ungroup()

                        } else {

                                max_value_vars <- all_numeric_cols(data = data)

                                data %>%
                                        dplyr::group_by({{ grouper }}) %>%
                                        dplyr::summarise_at(dplyr::vars(dplyr::all_of(max_value_vars)), max, na.rm = na.rm) %>%
                                        dplyr::ungroup()

                        }
                        
                        
                }

                
        }





#' @title
#' Summarize Numeric Columns with Standard Summary Functions
#' @param data A dataframe or tibble. 
#' @param ... column names for vectors of any data class
#' @param na.rm TRUE if true NA are to be removed. 
#' @param grouper (optional) Group by column.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{group_by}}
#' @rdname summarize_numeric
#' @example inst/examples/summarize_numeric.R
#' @export 
#' @importFrom dplyr enquos select summarize_at vars all_of group_by_at ungroup

summarize_numeric <-
        function(data, ..., grouper) {

                
                funs <-
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
                
                
                all_num_cols <- all_numeric_cols(data = data)
                
                if (!missing(...)) {
                        
                        cols <- dplyr::enquos(...)
                        selected_cols <- 
                                data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()
                        
                        num_vars <- selected_cols[selected_cols %in% all_num_cols]
                        
                        if (length(num_vars) == 0) {
                                
                                stop("columns are not numeric")
                                
                        }
                        
                } else {
                        
                        num_vars <- all_num_cols
                        
                        
                }
                
                if (missing(grouper)) {

                
                        data %>%
                                dplyr::summarize_at(dplyr::vars(dplyr::all_of(num_vars)),
                                                    funs)
                
                } else {
                        
                        
                        data %>%
                                dplyr::group_by_at(dplyr::vars({{ grouper }})) %>%
                                dplyr::summarize_at(dplyr::vars(dplyr::all_of(num_vars)),
                                                    funs) %>%
                                dplyr::ungroup()
                        
                }
        }





#' @title 
#' Get the number of times a row appears in a dataframe
#' @description 
#' Shortcut for `group_by_all()` followed by `count()`.
#'  \code{\link[cave]{vector_to_string}}
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}},\code{\link[dplyr]{group_by_all}}
#' @rdname summarize_rows
#' @export 
#' @importFrom cave vector_to_string
#' @importFrom dplyr enquos group_by summarize arrange ungroup desc group_by_at group_by_all %>% 

row_count <-
        function(data,
                 desc = TRUE) {
                
                
                if (desc) {
                        
                        data %>%
                                dplyr::group_by_all() %>%
                                dplyr::count() %>%
                                dplyr::ungroup() %>%
                                dplyr::arrange(desc(n))
                        
                } else {
                
                data %>%
                        dplyr::group_by_all() %>%
                        dplyr::count() %>%
                        dplyr::ungroup() %>%
                        dplyr::arrange(n)
                        
                        
                }
                
                

                
                
        }



#' @title 
#' Value Count for a Dataframe

#' @param data                  A dataframe or tibble.     
#' @param names_to              passed to tidyr pivot_longer()
#' @param values_to             passed to tidyr pivot_longer()
#' @seealso 
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[dplyr]{reexports}},\code{\link[dplyr]{count}},\code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{arrange}}
#' @rdname value_count
#' @family summarize functions
#' @export 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything count sym arrange

value_count <-
        function(data,
                 names_to = "Variable",
                 values_to = "Value",
                 desc = TRUE) {
   
                
                if (desc) {
                        
                        data %>%
                                mutate_all_char() %>%
                                tidyr::pivot_longer(cols = dplyr::everything(),
                                                    names_to = names_to,
                                                    values_to = values_to) %>%
                                dplyr::count(!!dplyr::sym(names_to), !!dplyr::sym(values_to)) %>%
                                        dplyr::arrange(desc(n))
                        
                        
                } else {
                        
                        data %>%
                                mutate_all_char() %>%
                                tidyr::pivot_longer(cols = dplyr::everything(),
                                                    names_to = names_to,
                                                    values_to = values_to) %>%
                                dplyr::count(!!dplyr::sym(names_to), !!dplyr::sym(values_to)) %>%
                                dplyr::arrange(n)
                        
                        
                }
                
                
                
        }


#' Summarize the counts of values in columns
#' @description (Deprecated) To group by more than 1 variable can be summarized using summarize_grouped_vars().
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
                
                
                .Deprecated(new = "value_count")
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



