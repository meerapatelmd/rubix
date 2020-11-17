

test_function <- 
        function(data,
                 grouper) {
                
                data %>%
                        mutate_all_char() %>%
                        tidyr::pivot_longer(cols = !{{ grouper }})
        }
