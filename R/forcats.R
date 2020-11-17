#' @title 
#' Categorize a Column 
#' 
#' @description 
#' Apply `forcats::fct_collapse` on a character column. 
#' 
#' @rdname categorize
#' @family forcats functions
#' @importFrom dplyr mutate_at vars 
#' @importFrom forcats fct_collapse fct_explicit_na
#' @export
#' @example inst/examples/forcats.R

categorize <-
        function(data, col, ..., other_values = NULL, na_level = "(Missing)") {
                
                
                if (is.null(na_level)) {

                        data %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_collapse, other_level = other_values, ...) %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}), as.character)
                        
                        
                } else {
                        
                        
                        data %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}), ~ forcats::fct_explicit_na(factor(.), na_level = na_level)) %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_collapse, other_level = other_values, `NA` = na_level, ...) %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}), as.character)
                        
                }
                
        }

#' @title 
#' Create a Categorizing Function 
#' 
#' @description 
#' Declare a function that applies `forcats::fct_collapse` on a character column. 
#' 
#' @rdname categorize_ff
#' @family factory functions
#' @importFrom dplyr mutate_at vars 
#' @importFrom forcats fct_collapse 
#' @export
#' @example inst/examples/forcats_ff.R

categorize_ff <-
        function(...) {
                
                
                function(data,
                         col) {
                        
                
                
                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_collapse, ...) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), as.character)
                
                }
        }

#' @title 
#' Recode a values to another
#' 
#' @description 
#' Apply `forcats::fct_recode` on a character column. 
#' 
#' @rdname recode_value
#' @family forcats functions
#' @importFrom dplyr mutate_at vars 
#' @importFrom forcats fct_recode
#' @export
#' @example inst/examples/forcats.R

recode_value <- 
        function(data, col, ...) {
                
                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_recode, ...) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), as.character)
                
        }

#' @title 
#' Recode a values to boolean
#' 
#' @description 
#' Apply `forcats::fct_recode` to a character column to convert to logical. 
#' 
#' @rdname recode_boolean
#' @family forcats functions
#' @importFrom dplyr mutate_at vars 
#' @importFrom forcats fct_recode
#' @export
#' @example inst/examples/forcats.R

recode_boolean <- 
        function(data, col, true_value, false_value) {
                
                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), factor) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), forcats::fct_collapse, `TRUE` = true_value, `FALSE` = false_value) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), as.logical)
                
        }




