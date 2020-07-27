#' Normalize a set of values to a true NA
#' @description This functions takes all the fields in a dataframe and replaces the "NA" string with NA_character_. If blanks is set to TRUE, blanks of character length of 0 are also replaced with NA_character_.
#' @import dplyr
#' @import stringr
#' @export



normalize_all_to_na <- 
        function(.data, blanks = TRUE) {
                
                .data <- mutate_na_str_to_na(.data = .data)

                if (blanks) {
                        
                        .data <- 
                                mutate_blank_to_na(.data = .data)
                        
                }
                
                return(.data)
        }