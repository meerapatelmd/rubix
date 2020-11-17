#' @title 
#' Summarize Functions
#' @param data A dataframe or tibble. 
#' @param incl_num_calc If TRUE, includes an additional dataframe of summary statistics on the numeric columns in the dataframe.
#' 
#' @name summarize_functions
NULL




#' Summarize a Variable
#' @param data A dataframe or tibble. 
#' @param incl_num_calc If TRUE, includes an additional dataframe of summary statistics on the numeric columns in the dataframe.
#' @rdname summarize_variables
#' @seealso 
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[dplyr]{reexports}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{select}}
#' @rdname summarize_variables
#' @family summary functions
#' @example inst/examples/summarize_.R
#' @export 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything group_by_at vars all_of summarize_at select

summarize_variables <-
        function(data, 
                 incl_num_calc = TRUE,
                 names_to = "Variable",
                 values_to = "Value",
                 grouper) {
                
                char_funs <- list(COUNT = ~ length(.),
                                  DISTINCT_COUNT = ~ length(unique(.)),
                                  NA_COUNT = ~ length(.[is.na(.)]),
                                  NA_STR_COUNT = ~ length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]),
                                  BLANK_COUNT = ~ length(.[. %in% c("")]),
                                  DISTINCT_VALUES = ~ paste(unique(as.character(.)), collapse="|"))
                
                if (missing(grouper)) {
                
                        
                        main_output <-
                                data %>%
                                mutate_all_char()  %>% 
                                tidyr::pivot_longer(cols = dplyr::everything(),
                                                    names_to = names_to,
                                                    values_to = values_to,
                                                    values_drop_na = FALSE)  %>%
                                dplyr::group_by_at(dplyr::vars(dplyr::all_of(names_to)))  %>%
                                dplyr::summarize_at(dplyr::vars(dplyr::all_of(values_to)),
                                                    char_funs)
                        
                        
                } else {
                        
                        
                        main_output <-
                                data %>%
                                mutate_all_char()  %>% 
                                tidyr::pivot_longer(cols = !{{ grouper }},
                                                    names_to = names_to,
                                                    values_to = values_to,
                                                    values_drop_na = FALSE)  %>%
                                dplyr::group_by_at(dplyr::vars({{ grouper }}, 
                                                               dplyr::all_of(names_to)))  %>%
                                dplyr::summarize_at(dplyr::vars(dplyr::all_of(values_to)),
                                                    char_funs)
                        
                        
                        
                        
                }
                
                
                
                if (incl_num_calc) {      
                        
                        all_nums <- all_numeric_cols(data = data)
                        
                        numeric_funs <-
                                list(MEAN = ~mean(., na.rm = TRUE), 
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
                                     DISTINCT_STR_NA = ~paste(sort(unique(as.character(.)) %>% no_na()), collapse = "|"))
                        
                        
                        if (missing(grouper)) {
                        
                                
                                numeric_output <- 
                                        data %>%
                                        dplyr::select(dplyr::all_of(all_nums)) %>%
                                        tidyr::pivot_longer(cols = dplyr::everything(),
                                                            names_to = names_to,
                                                            values_to = values_to,
                                                            values_drop_na = FALSE)  %>%
                                        dplyr::group_by_at(dplyr::vars(dplyr::all_of(names_to)))  %>%
                                        dplyr::summarize_at(dplyr::vars(dplyr::all_of(values_to)),
                                                            numeric_funs)
                                
                                
                        } else {
                                
                                
                                numeric_output <- 
                                        data %>%
                                        dplyr::select({{ grouper }}, dplyr::all_of(all_nums)) %>%
                                        tidyr::pivot_longer(cols = !{{ grouper }},
                                                            names_to = names_to,
                                                            values_to = values_to,
                                                            values_drop_na = FALSE)  %>%
                                        dplyr::group_by_at(dplyr::vars({{ grouper }}, dplyr::all_of(names_to)))  %>%
                                        dplyr::summarize_at(dplyr::vars(dplyr::all_of(values_to)),
                                                            numeric_funs)
                                
                                
                                
                        }
                                
                                
                                
                        
                        
                        list(SUMMARY = main_output,
                             NUMERIC_CALCULATIONS = numeric_output)
                        
                        
                        
                        
                } else {
                        
                        main_output
                        
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
#' Not to be confused with a total row count of a dataframe (ie `nrow()`), this is a shortcut for `group_by_all()` followed by `count()`. 
#' @return 
#' Ungrouped dataframe with all input columns with the addition of an `n` column for the count.
#' @seealso 
#'  \code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{count}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{arrange}}
#' @rdname observation_count
#' @family summary functions
#' @export 
#' @importFrom dplyr group_by_all count ungroup arrange

observation_count <-
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
                 desc = TRUE,
                 grouper) {
   
                if (missing(grouper)) {
                
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
                        
                } else {
                        
                        
                        if (desc) {
                                
                                data %>%
                                        mutate_all_char() %>%
                                        tidyr::pivot_longer(cols = !{{ grouper }},
                                                            names_to = names_to,
                                                            values_to = values_to) %>%
                                        dplyr::count({{ grouper }}, !!dplyr::sym(names_to), !!dplyr::sym(values_to)) %>%
                                        dplyr::arrange(desc(n))
                                
                                
                        } else {
                                
                                data %>%
                                        mutate_all_char() %>%
                                        tidyr::pivot_longer(cols = !{{ grouper }},
                                                            names_to = names_to,
                                                            values_to = values_to) %>%
                                        dplyr::count({{ grouper }}, !!dplyr::sym(names_to), !!dplyr::sym(values_to)) %>%
                                        dplyr::arrange(n)
                                
                                
                        }
                        
                        
                        
                        
                        
                }
                
                
                
        }



