




summarize_logical <- 
        function(.data,
                 ...,
                 names_to = "VARIABLE",
                 values_to = "VALUE") {
                
                cols <- enquos(...)
                
                group_counts <- summarize_values(.data,
                                                 ...,
                                                 names_to = names_to,
                                                 values_to = values_to)
                
                group_counts %>%
                        dplyr::filter_at(vars(!!!cols),
                                         any_vars(!is.na(.)))
        }