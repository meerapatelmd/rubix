#' Normalize a set of values to a true NA
#' @desciption This functions takes all the fields in a dataframe and replaces the "NA" string with NA_character_. If blanks is set to TRUE, blanks of character length of 0 are also replaced with NA_character_.
#' @import dplyr
#' @import stringr
#' @export

normalize_at_to_na <- 
        function(.data, at_col, blanks = TRUE) {
                
                at_col <- enquo(at_col)
                
                .data <-
                .data %>%
                        dplyr::mutate_at(vars(!!at_col), stringr::str_replace_all, "^NA$", NA_character_) 
                
                if (blanks) {
                        
                        .data <- 
                                .data %>%
                                dplyr::mutate_at(vars(!!at_col), stringr::str_replace_all, "^$", NA_character_)
                        
                }
                
                return(.data)
        }