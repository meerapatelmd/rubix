
paste_columns_with_na_rm <-
        function(.data, new_col_name, ..., collapse = " ", sep = "") {
                new_col_name <- enquo(new_col_name)
                paste_vars   <- enquos(...)
                
                if (collapse == " ") {
                        return(
                                .data %>%
                                        mutate_at(vars(!!!paste_vars), list(~ ifelse(is.na(.), "", .))) %>%
                                        mutate(!!new_col_name := paste(!!!paste_vars)) 
                        )
                } else {
                        return(
                                .data %>%
                                        mutate_at(vars(!!!paste_vars), list(~ ifelse(is.na(.), "", .))) %>%
                                        mutate(!!new_col_name := paste(!!!paste_vars)) %>%
                                        mutate_at(vars(!!new_col_name), funs(gsub(" ", collapse, .)))
                        )
                }
        }
