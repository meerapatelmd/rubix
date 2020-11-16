#' Summarize a Variable
#' @param incl_num_calc If TRUE, includes an additional dataframe of summary statistics on the numeric columns in the dataframe.
#' @rdname summarize_variables
#' @seealso 
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[dplyr]{reexports}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{select}}
#' @rdname summarize_variables
#' @family summary functions
#' @example inst/examples/summarize_variables.R
#' @export 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything group_by_at vars all_of summarize_at select

summarize_variables <-
        function(data, 
                 incl_num_calc = TRUE,
                 names_to = "Variable",
                 values_to = "Value") {
                
                char_funs <- list(COUNT = ~ length(.),
                                  DISTINCT_COUNT = ~ length(unique(.)),
                                  NA_COUNT = ~ length(.[is.na(.)]),
                                  NA_STR_COUNT = ~ length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]),
                                  BLANK_COUNT = ~ length(.[. %in% c("")]),
                                  DISTINCT_VALUES = ~ paste(unique(as.character(.)), collapse="|"))
                
                
                
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
                                
                                
                                list(SUMMARY = main_output,
                                     NUMERIC_CALCULATIONS = numeric_output)
                                
                                
                                
                                
                } else {
                                
                        main_output
                        
                }
                
                
        }
