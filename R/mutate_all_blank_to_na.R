#' Replace "" with <NA>
#' @return A tibble
#' @importFrom tibble as_tibble
#' @export

mutate_all_blank_to_na <- 
        function(.data) {
                .data <-
                        .data %>%
                        tibble::as_tibble()
                
                .data[.data == ""] <- NA_character_
                return(.data)
        }