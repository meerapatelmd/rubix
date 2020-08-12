#' Normalize a set of values to a true NA
#' @description 
#' This functions takes all the fields in a dataframe and replaces the "NA" string with NA_character_. If blanks is set to TRUE, blanks of character length of 0 are also replaced with NA_character_.
#' @seealso 
#'  \code{\link[dplyr]{mutate_all}}
#' @rdname normalize_at_to_na
#' @export 
#' @importFrom dplyr mutate_at %>%

normalize_at_to_na <- 
        function(.data, at_col, blanks = TRUE) {
                
                str_replace_na_string <-
                        function(vector) {
                                vector[vector %in% c("NA")] <- NA
                                return(vector)
                        }
                
                str_replace_blank <-
                        function(vector) {
                                vector[vector %in% c("")] <- NA
                                return(vector)
                        }
                
                at_col <- enquo(at_col)
                
                .data <-
                .data %>%
                        dplyr::mutate_at(vars(!!at_col), str_replace_na_string) 
                
                if (blanks) {
                        
                        .data <- 
                                .data %>%
                                dplyr::mutate_at(vars(!!at_col), str_replace_blank)
                        
                }
                
                return(.data)
        }