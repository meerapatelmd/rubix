#' @title 
#' Aggregate Function
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}}
#' @rdname group_by_aggregate
#' @export 
#' @importFrom dplyr enquos enquo group_by summarize_at ungroup %>%

group_by_aggregate <- 
                function(dataframe, 
                         ...,
                         agg.col,
                         collapse = "|") {
                        
                                
                                        group_by_cols <- dplyr::enquos(...) 
                                        agg.col <- dplyr::enquo(agg.col)
                                        
                                        
                                        dataframe %>%
                                                dplyr::group_by(!!!group_by_cols) %>%
                                                dplyr::summarize_at(vars(!!agg.col), function(x) paste(x, collapse = collapse)) %>%
                                                dplyr::ungroup()
                
                
                
                }
