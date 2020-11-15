#' @title 
#' Arrange a Column by Character Number 
#' @description 
#' Order a dataframe based on the number of characters in the column provided as an argument.
#' @param col     column where number of characters will determine the order
#' @param desc          Should the output be in descending or ascending order?
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{select}}
#' @rdname arrange_by_nchar
#' @export 
#' @importFrom dplyr enquo mutate arrange select %>%

arrange_by_nchar <-
        function(data, col, desc = FALSE) {
                

                if (desc == FALSE) {
                                data %>%
                                dplyr::arrange_at(vars({{ col }}),
                                                  ~ nchar(as.character(.))
                                                  )
                } else {
                        data %>%
                                dplyr::arrange_at(vars({{ col }}),
                                                  ~ desc(nchar(as.character(.)))
                                )
                }

        }
