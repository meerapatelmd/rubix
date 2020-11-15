#' @export

grep_empty <- 
        function(vector) {
                
                grep(pattern = "^$",
                      x = vector)
                
        }

#' @export

grep_blank <- 
        function(vector) {
                
                grep(pattern = "^[ ]{1,}$",
                     x = vector)
                
        }

#' @export

grep_na_str <- 
        function(vector) {
                
                grep(pattern = "^NA$",
                     x = vector)
                
        }

#' @export

empty_to_na <- 
        function(vector) {
                
                
                if (is.character(vector)) {
                
                vector[grep_empty(vector)] <- NA_character_
                
                }
                
                vector
                
                
        }

#' @export

blank_to_na <- 
        function(vector) {
                
                
                if (is.character(vector)) {
                        
                        vector[grep_blank(vector)] <- NA_character_
                        
                }
                
                vector
                
                
        }

#' @export

na_str_to_na <- 
        function(vector) {
                
                
                if (is.character(vector)) {
                        
                        vector[grep_na_str(vector)] <- NA_character_
                        
                }
                
                vector
                
        }



#' @title 
#' Normalize Character Column Values to `NA_character_``
#' 
#' @param data A dataframe or tibble. 
#' @param blank Should strings consisting with one or more spaces be replaced with NA_character_?
#' @param empty Should strings with a length of 0 be replaced with NA_character_?
#' @param na_str Should "NA" be replaced with NA_character_?
#' 
#' @example inst/examples/normalize_na.R
#' @seealso 
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}}
#' @rdname mutate_all_na_character
#' @export 
#' @importFrom dplyr mutate_at vars all_of

mutate_all_na_character <- 
        function(data,
                 blank = TRUE,
                 empty = TRUE,
                 na_str = TRUE) {
                
                cols <- char_cols(data = data)
                
                if (blank) {
                        
                        data <- 
                                data %>%
                                dplyr::mutate_at(dplyr::vars(dplyr::all_of(cols)),
                                                 ~ blank_to_na(.)
                                                 )
                        
                }
                
                if (empty) {
                        
                        
                        data <- 
                                data %>%
                                dplyr::mutate_at(dplyr::vars(dplyr::all_of(cols)),
                                                 ~ empty_to_na(.)
                                )
                        
                }
                
                
                if (na_str) {
                        
                        
                        data <- 
                                data %>%
                                dplyr::mutate_at(dplyr::vars(dplyr::all_of(cols)),
                                                 ~ na_str_to_na(.)
                                )
                        
                }
                
                
                data
                
                
        }


