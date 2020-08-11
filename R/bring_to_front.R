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
#' @importFrom dplyr enquos select everything


bring_to_front <-
        function(.data, ...) {
                cols <- dplyr::enquos(...)
                .data %>% 
                        dplyr::select(!!!cols, dplyr::everything())
        }

