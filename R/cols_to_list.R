#' Select cols and convert them to a list
#' @description This function takes a dataframe and breaks it down into a list by column. The columns can be explicitly provided or if ... is empty, the entire dataframe will be broken down. 
#' @param id_column_name optional string of the column designated as the identifier that should remain in the output. 
#' @param named the returned list is named with the column name
#' @importFrom dplyr enquos
#' @importFrom dplyr select
#' @importFrom purrr map
#' @export

cols_to_list <- 
        function(dataframe, ..., id_column_name = NULL, named = TRUE) {
                if (!missing(...)) {
                
                                if (is.null(id_column_name)) {
                                        
                                                cols <- dplyr::enquos(...)
                                                
                                                dataframe <-
                                                        dataframe %>%
                                                        dplyr::select(!!!cols)
                                                
                                                column_names <- colnames(dataframe)
                                                
                                                output <- 
                                                        column_names %>%
                                                        purrr::map(function(x) dataframe %>%
                                                                           dplyr::select(all_of(x)))
                                                
                                                
                                                
                                } else {
                                        
                                                id_column_name <- enquo(id_column_name)
                                                cols <- dplyr::enquos(...)
                                                
                                                dataframe <-
                                                        dataframe %>%
                                                        dplyr::select(!!id_col, !!!cols)
                                                
                                                column_names <- 
                                                        dataframe %>%
                                                        dplyr::select(!!!cols) %>%
                                                        colnames()
                                                
                                                output <- 
                                                        column_names %>%
                                                        purrr::map(function(x) dataframe %>%
                                                                           dplyr::select(!!id_col, all_of(x)))
                                                
                                                
                                }
                } else {
                        
                        if (is.null(id_column_name)) {
                                
                                                column_names <- colnames(dataframe)
                                                
                                                output <- 
                                                        column_names %>%
                                                        purrr::map(function(x) dataframe %>%
                                                                           dplyr::select(all_of(x)))
                                                
                                                
                                                
                        } else {
                                                
                                                id_column_name <- dplyr::enquo(id_column_name)
                                                
                                                column_names <-
                                                dataframe %>%
                                                        dplyr::select(-!!id_column_name) %>%
                                                        colnames()
                                                
                                                
                                                output <- 
                                                        column_names %>%
                                                        purrr::map(function(x) dataframe %>%
                                                                           dplyr::select(!!id_col, all_of(x)))
                                                
                                               
                                                
                                                
                        }

                }
                
                if (named == TRUE) {
                        names(output) <- column_names
                }
                
                return(output)
                        
        }
