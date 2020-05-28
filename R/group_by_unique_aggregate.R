#' Aggregate Function
#' @importFrom dplyr enquos
#' @importFrom dplyr enquo
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_at
#' @importFrom dplyr ungroup
#' @export

group_by_unique_aggregate <- 
                function(dataframe, 
                         ...,
                         agg.col,
                         collapse = "|") {
                        
                                
                                        group_by_cols <- dplyr::enquos(...) 
                                        agg.col <- dplyr::enquo(agg.col)
                                        
                                        
                                        dataframe %>%
                                                dplyr::group_by(!!!group_by_cols) %>%
                                                dplyr::summarize_at(vars(!!agg.col), function(x) paste(unique(x), collapse = collapse)) %>%
                                                dplyr::ungroup()
                
                
                
                }
