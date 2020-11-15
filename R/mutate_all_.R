




mutate_all_char <- 
        function(data) {
                
                data %>%
                        dplyr::mutate_all(as.character)
                
        }


mutate_all_trimws <- 
        function(data,
                 which = "both") {
                
                cols <- char_cols(data)
                
                
                data %>%
                        dplyr::mutate_at(vars(dplyr::all_of(cols)),
                                         ~ trimws(., which = which))
                
                
                
        }
