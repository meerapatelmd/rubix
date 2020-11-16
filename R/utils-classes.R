#' @export


col_classes <- 
        function(data) {
                
                lapply(data, class)
                
        }


#' @export


col_class <- 
        function(data, col) {
                
                data %>%
                        dplyr::select({{ col }}) %>%
                        unlist() %>%
                        class()
                
        }


#' @export

class_cols_ff <- 
        function(class) {
                
                function(data) {
                        
                        col_classes(data) %>%
                                purrr::keep(~ . ==  class) %>%
                                names()
                        
                        
                }
                
                
        }


#' @export

char_cols <- 
        class_cols_ff(class = "character")


#' @export

int_cols <- 
        class_cols_ff(class = "integer")

#' @export

dbl_cols <- 
        class_cols_ff(class = "double")


#' @export

num_cols <- 
        class_cols_ff(class = "numeric")


#' @export

all_numeric_cols <- 
        function(data) {
                unique(c(int_cols(data), 
                         dbl_cols(data), 
                         num_cols(data)))
        }


#' @export

lgl_cols <- 
        class_cols_ff(class = "logical")

