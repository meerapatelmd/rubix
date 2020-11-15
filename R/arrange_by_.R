#' @title 
#' Arrange a Column by Character Number 
#' @description 
#' Order a dataframe based on the number of characters in the column provided as an argument.
#' @inheritParams wrapper_args
#' @param desc Return sorted in descending order? 
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames only.
#' @seealso 
#'  \code{\link[dplyr]{arrange_all}}
#' @rdname arrange_by_nchar
#' @export 
#' @importFrom dplyr arrange_at

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
