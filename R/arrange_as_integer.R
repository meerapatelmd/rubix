#' @title 
#' Arrange by a Given Column as Integer
#' @description 
#' Perform an arrange function call on a dataframe with the values of the target column as an integer class.
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
                
                column <- dplyr::enquo(column)
                

                if (desc == FALSE) {
                        .data %>%
                                dplyr::arrange(as.integer(!!column))
                } else {
                        .data %>%
                                dplyr::arrange(desc(as.integer(!!column)))
                }

        }
