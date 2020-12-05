


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vector PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname all_is_na
#' @export 
all_is_na <- 
        function(vector) {
                
                
                all(is.na(vector))
                
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vector PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname all_not_na
#' @export 
all_not_na <- 
        function(vector) {
                
                
                !all_is_na(vector)
                
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vector PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname any_is_na
#' @export 
any_is_na <- 
        function(vector) {
                
                
                any(is.na(vector))
                
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vector PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname all_same_value
#' @export 
all_same_value <- 
        function(vector, 
                 na.rm = FALSE) {
                
                if (na.rm) {
                        vector <- vector[!(is.na(vector))]
                }
                
                length(unique(vector)) == 1
        }
