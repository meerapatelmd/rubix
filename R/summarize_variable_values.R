#' Summarize a variable by distinct counts of values it has
#' @param ... grouping vars. If missing, all variables will be counted
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr enquos
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @export


summarize_variable_values <-
        function(dataframe, ..., as_expr = FALSE, collapse = "|") {

                if (!missing(...)) {
                        
                                cols <- dplyr::enquos(...)
                                
                                if (as_expr == FALSE) {
                                        output_1 <-
                                                dataframe %>%
                                                dplyr::summarize_at(vars(!!!cols), 
                                                                    list(VALUES = function(x) unique(x) %>%
                                                                                 paste(collapse = collapse)))
                                        
                                } else {
                                        
                                        output_1 <-
                                                dataframe %>%
                                                dplyr::summarize_at(vars(!!!cols), 
                                                                    list(VALUES = function(x) unique(x) %>%
                                                                                 cave::vector_to_string()))
                                }
                                
                                
                        
                } else {
                        
                        
                        if (as_expr == FALSE)  {
                                
                                output_1 <-
                                        dataframe %>%
                                        dplyr::summarize_at(vars(everything()), list(VALUES = function(x) unique(x) %>%
                                                                                             paste(collapse = collapse)))
                                
                        } else {
                                
                                output_1 <-
                                        dataframe %>%
                                        dplyr::summarize_at(vars(everything()), list(VALUES = function(x) unique(x) %>%
                                                                                             cave::vector_to_string()))
                                
                        }
                        
                }
                
                output_2 <- 
                        output_1 %>%
                        tidyr::pivot_longer(cols = everything(),
                                            names_pattern = "(^.*?)[_](.*$)",
                                            names_to = c("Variable", "Metric Type"),
                                            values_to = "Metric") %>%
                        tidyr::pivot_wider(id_cols = Variable,
                                           names_from = `Metric Type`,
                                           values_from = Metric)
                                
                
                return(output_2)
        }
