#' Aggregate column into a single string
#' @param data dataframe
#' @param col unquoted column to aggregate
#' @param collapse collapse argument for the paste function
#' @importFrom stats aggregate
#' @importFrom rlang enquo
#' @export


aggregate_col <-
        function(data,
                 col,
                 collapse) {
                
                        col <- rlang::enquo(col)
                        
                        stats::aggregate(!!col ~ .,
                                  data = data,
                                  paste,
                                  collapse = collapse)
        }

