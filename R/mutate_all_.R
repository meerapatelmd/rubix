#' @title 
#' Mutate All Columns to Character
#' 
#' @param data A dataframe or tibble. 
#' @example inst/examples/mutate_all_.R
#' @seealso 
#'  \code{\link[dplyr]{mutate_all}}
#' @rdname mutate_all_char
#' @export 
#' @importFrom dplyr mutate_all

mutate_all_char <- 
        function(data) {
                
                data %>%
                        dplyr::mutate_all(as.character)
                
        }


#' @title 
#' Trim Whitespace of all Character Columns
#' 
#' @param data A dataframe or tibble. 
#' @inheritParams base::trimws
#' 
#' @example inst/examples/mutate_all_.R
#' @seealso 
#'  \code{\link[dplyr]{mutate_at}}
#' @rdname mutate_all_trimws
#' @export 
#' @importFrom dplyr mutate_at all_of


mutate_all_trimws <- 
        function(data,
                 which = c("both", "left", "right"),
                 whitespace = "[ \t\r\n]") {
                
                cols <- char_cols(data)
                
                
                data %>%
                        dplyr::mutate_at(vars(dplyr::all_of(cols)),
                                         ~ trimws(., 
                                                  which = which,
                                                  whitespace = whitespace))
                
                
                
        }
