#' @title 
#' Arrange by a Column as Integer
#' @inheritParams wrapper_args
#' @param desc Return sorted in descending order? 
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames only.
#' @example inst/examples/arrange_.R
#' @seealso 
#'  \code{\link[dplyr]{arrange_at}},\code{\link[dplyr]{vars}}
#' @rdname arrange_int
#' @family arrange functions
#' @export 
#' @importFrom dplyr arrange_at vars


arrange_int <-
        function(data, 
                 col, 
                 desc = FALSE,
                 .by_group = FALSE) {

                if (desc == FALSE) {
                        
                       data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ as.integer(.),
                                                  .by_group = .by_group)
                        
                } else {
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ desc(as.integer(.)),
                                                  .by_group = .by_group
                                )
                }

        }


#' @title 
#' Arrange by a Given Column as Numeric
#' @inheritParams wrapper_args
#' @param desc Return sorted in descending order? 
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames only.
#' @example inst/examples/arrange_.R
#' @rdname arrange_num
#' @family arrange functions
#' @seealso 
#'  \code{\link[dplyr]{arrange_at}},\code{\link[dplyr]{vars}}
#' @export 
#' @importFrom dplyr arrange_at vars


arrange_num <-
        function(data, col, desc = FALSE, .by_group = FALSE) {
                
                if (desc == FALSE) {
                        
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ as.numeric(.),
                                                  .by_group = .by_group
                                )
                        
                } else {
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ desc(as.numeric(.)),
                                                  .by_group = .by_group
                                )
                }
                
        }


#' @title 
#' Arrange by a Given Column as Double
#' @inheritParams wrapper_args
#' @param desc Return sorted in descending order? 
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames only.
#' @example inst/examples/arrange_.R
#' @rdname arrange_dbl
#' @family arrange functions
#' @seealso 
#'  \code{\link[dplyr]{arrange_at}},\code{\link[dplyr]{vars}}
#' @export 
#' @importFrom dplyr arrange_at vars


arrange_dbl <-
        function(data, col, desc = FALSE, .by_group = FALSE) {
                
                if (desc == FALSE) {
                        
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ as.double(.),
                                                  .by_group = .by_group
                                )
                        
                } else {
                        data %>%
                                dplyr::arrange_at(dplyr::vars({{ col }}),
                                                  ~ desc(as.double(.)),
                                                  .by_group = .by_group
                                )
                }
                
        }
