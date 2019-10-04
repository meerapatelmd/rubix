
row_one_to_column_names <-
        function(dataframe) {
                new_col_names <- dataframe %>%
                                        dplyr::filter(row_number() == 1) %>%
                                        unname() %>%
                                        unlist() %>%
                                        as.character()
                x <- assign_new_col_names(dataframe, new_col_names = new_col_names)
                x <- x[-1,]
                return(x)
        }
