




mutate_na_str_to_na <- 
        function(.data) {
                .data <-
                        .data %>%
                        tibble::as_tibble()
                
                .data[.data == "NA"] <- NA_character_
                return(.data)
        }