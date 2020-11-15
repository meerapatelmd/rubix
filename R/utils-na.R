
#' @export


all_is_na <- 
        function(vector) {
                
                
                all(is.na(vector))
                
        }

#' @export

all_not_na <- 
        function(vector) {
                
                
                !all_is_na(vector)
                
        }

#' @export

any_is_na <- 
        function(vector) {
                
                
                any(is.na(vector))
                
        }

#' @export

all_same_value <- 
        function(vector, 
                 na.rm = FALSE) {
                
                if (na.rm) {
                        vector <- vector[!(is.na(vector))]
                }
                
                length(unique(vector)) == 1
        }
