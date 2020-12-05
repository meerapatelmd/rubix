
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
#' @rdname grep_empty
#' @export 
grep_empty <- 
        function(vector) {
                
                grep(pattern = "^$",
                     x = vector)
                
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
#' @rdname grep_blank
#' @export 
grep_blank <- 
        function(vector) {
                
                grep(pattern = "^[ ]{1,}$",
                     x = vector)
                
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
#' @rdname grep_na_str
#' @export 
grep_na_str <- 
        function(vector) {
                
                grep(pattern = "^NA$",
                     x = vector)
                
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
#' @rdname empty_to_na
#' @export 
empty_to_na <- 
        function(vector) {
                
                
                if (is.character(vector)) {
                        
                        vector[grep_empty(vector)] <- NA_character_
                        
                }
                
                vector
                
                
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
#' @rdname blank_to_na
#' @export 
blank_to_na <- 
        function(vector) {
                
                
                if (is.character(vector)) {
                        
                        vector[grep_blank(vector)] <- NA_character_
                        
                }
                
                vector
                
                
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
#' @rdname na_str_to_na
#' @export 
na_str_to_na <- 
        function(vector) {
                
                
                if (is.character(vector)) {
                        
                        vector[grep_na_str(vector)] <- NA_character_
                        
                }
                
                vector
                
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





