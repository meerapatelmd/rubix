


extract_to_df <-
        function(data,
                 col,
                 pattern =  "\\[(.*)\\] (.*?): (.*$)",
                 names   = c("commit",
                             "commit_date",
                             "commit_description")) {
                
                
                output <- list()
                
                for (i in seq_along(names)) {
                        
                        new_col_contents <-
                                stringr::str_replace_all(
                                        string = x,
                                        pattern = pattern,
                                        replacement = as.character(glue::glue("\\{i}"))
                                )
                        
                        output[[i]] <- new_col_contents
                        names(output)[i] <- names[i]
                        
                }
                
                output %>%
                        tibble::as_tibble()
                
        }