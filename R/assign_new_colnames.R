
assign_new_col_names <-
        function(dataframe, new_col_names) {
                dataframe_obj_name <- deparse(substitute(dataframe))
                
                x <- dataframe %>%
                        unname()
                
                colnames(x) <- new_col_names
                
                return(x)

        }