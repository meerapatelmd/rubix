

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname col_classes
#' @export 
col_classes <- 
        function(data) {
                
                lapply(data, class)
                
        }




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{select}}
#' @rdname col_class
#' @export 
#' @importFrom dplyr select
col_class <- 
        function(data, col) {
                
                data %>%
                        dplyr::select({{ col }}) %>%
                        unlist() %>%
                        class()
                
        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param class PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{keep}}
#' @rdname class_cols_ff
#' @export 
#' @importFrom purrr keep
class_cols_ff <- 
        function(class) {
                
                function(data) {
                        
                        col_classes(data) %>%
                                purrr::keep(~ . ==  class) %>%
                                names()
                        
                        
                }
                
                
        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{keep}}
#' @rdname char_cols
#' @export 
#' @importFrom purrr keep
char_cols <- 
        class_cols_ff(class = "character")



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{keep}}
#' @rdname int_cols
#' @export 
#' @importFrom purrr keep
int_cols <- 
        class_cols_ff(class = "integer")


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{keep}}
#' @rdname dbl_cols
#' @export 
#' @importFrom purrr keep
dbl_cols <- 
        class_cols_ff(class = "double")



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{keep}}
#' @rdname num_cols
#' @export 
#' @importFrom purrr keep
num_cols <- 
        class_cols_ff(class = "numeric")



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname all_numeric_cols
#' @export 
all_numeric_cols <- 
        function(data) {
                unique(c(int_cols(data), 
                         dbl_cols(data), 
                         num_cols(data)))
        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{keep}}
#' @rdname lgl_cols
#' @export 
#' @importFrom purrr keep
lgl_cols <- 
        class_cols_ff(class = "logical")

