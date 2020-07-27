#' Mutate all "NA" to <NA>
#' @return A tibble
#' @importFrom tibble as_tibble
#' @export

mutate_all_na_str_to_na <- 
        function(.data) {
                .data <-
                        .data %>%
                        tibble::as_tibble()
                
                .data[.data == "NA"] <- NA_character_
                return(.data)
        }