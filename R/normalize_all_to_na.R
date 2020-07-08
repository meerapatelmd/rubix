#' Normalize a set of values to a true NA
#' @desciption This functions takes all the fields in a dataframe and replaces the "NA" string with NA_character_. If blanks is set to TRUE, blanks of character length of 0 are also replaced with NA_character_.
#' @import dplyr
#' @import stringr
#' @export



normalize_all_to_na <- 
        function(.data, blanks = TRUE) {
                .data <-
                .data %>%
                        dplyr::mutate_all(stringr::str_replace_all, "^NA$", NA_character_) 
                
                if (blanks) {
                        
                        .data <- 
                                .data %>%
                                dplyr::mutate_all(stringr::str_replace_all, "^$", NA_character_)
                        
                }
                
                return(.data)
        }