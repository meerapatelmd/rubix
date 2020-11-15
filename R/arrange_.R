#' @title 
#' Arrange by a Given Column as Integer
#' @inheritParams wrapper_args
#' @param desc Return sorted in descending order? 
#' @example inst/examples/arrange_int.R
#' @rdname arrange_as_integer
#' @export 
#' @importFrom dplyr enquo arrange %>%


arrange_int <-
        function(data, col, desc = FALSE) {

                if (desc == FALSE) {
                        
                       data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ as.integer(.)
                                                  )
                        
                } else {
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ desc(as.integer(.))
                                )
                }

        }


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


arrange_num <-
        function(data, col, desc = FALSE) {
                
                if (desc == FALSE) {
                        
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ as.numeric(.)
                                )
                        
                } else {
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ desc(as.numeric(.))
                                )
                }
                
        }


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


arrange_dbl <-
        function(data, col, desc = FALSE) {
                
                if (desc == FALSE) {
                        
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ as.double(.)
                                )
                        
                } else {
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ desc(as.double(.))
                                )
                }
                
        }
