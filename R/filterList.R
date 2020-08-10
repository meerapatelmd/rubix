#' @title Filter by a list or vector
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#'                      
#' if(interactive()){
#' testDf <- data.frame(A = 1:3,
#'                      B = 4:6)
#'                      
#' print(testDf)
#' 
#' applyFilters <- list("A == 2",
#'                      "B == 5") 
#'                      
#' testDf %>% 
#'     filterList(applyFilters)
#'  }
#' @seealso 
#'  \code{\link[rlang]{list2}},\code{\link[rlang]{parse_expr}}
#' @rdname filterList
#' @export 
#' @importFrom rlang list2 parse_expr
#' @importFrom dplyr filter %>% 


filterList <-
        function(.data, ...) {
                
                newFilters <- paste(unlist(rlang::list2(...)), collapse = ", ")
                
                eval(
                        rlang::parse_expr(
                                paste0(
                                        ".data %>%
                        dplyr::filter(",newFilters, ")")))
        }