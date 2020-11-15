#' @title 
#' Arrange by a Given Column as Integer
#' @description 
#' (Deprecated) Perform an arrange function call on a dataframe with the values of the target column as an integer class.
#' @param .data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION
#' @param desc PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{arrange}}
#' @rdname arrange_as_integer
#' @export 
#' @importFrom dplyr enquo arrange %>%


arrange_as_integer <-
        function(.data, column, desc = FALSE) {
                
                .Deprecated(new = "arrange_as_int")
                
                column <- dplyr::enquo(column)
                

                if (desc == FALSE) {
                        .data %>%
                                dplyr::arrange(as.integer(!!column))
                } else {
                        .data %>%
                                dplyr::arrange(desc(as.integer(!!column)))
                }

        }





#' @title Bring Columns to the Front
#' @description  Bring a vector of columns to the front of a dataframe. 
#' @param .data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname bring_to_front
#' @export 
#' @importFrom dplyr enquos select everything %>%


bring_to_front <-
        function(.data, ...) {
                
                .Deprecated()
                cols <- dplyr::enquos(...)
                .data %>% 
                        dplyr::select(!!!cols, dplyr::everything())
        }






#' @title General Dataframe Cleanup
#' @description
#' Convert all columns to character class and trim all left and right white spaces.
#' @param dataframe input dataframe
#' @importFrom dplyr mutate_all
#' @importFrom dplyr %>% 
#' @export

call_mr_clean <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character) %>%
                dplyr::mutate_all(trimws, "both")
}






#' Filter for a row n
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @export

filter_row_n <-
        function(.data, n, invert = FALSE) {
                
                .Deprecated()
                if (invert) {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() != n)
                        

                } else {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() == n)
                        
                        
                }

        }





