#' @title 
#' Summarize groups by count
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}}
#' @rdname summarize_grouped_n
#' @export 
#' @importFrom dplyr enquos group_by summarize arrange ungroup desc %>%


summarize_grouped_n <-
        function(dataframe,
                 ...,
                 desc = FALSE) {
                
                cols <- dplyr::enquos(...)
                
                if (desc == FALSE) {
                        dataframe %>%
                                dplyr::group_by(!!!cols) %>%
                                dplyr::summarize(n = n()) %>%
                                dplyr::arrange(n) %>%
                                dplyr::ungroup()
                } else {
                        dataframe %>%
                                dplyr::group_by(!!!cols) %>%
                                dplyr::summarize(n = n()) %>%
                                dplyr::arrange(dplyr::desc(n)) %>%
                                dplyr::ungroup()
                }
                
        }