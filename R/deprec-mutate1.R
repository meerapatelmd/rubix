#' Mutate a column to position 1
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export


mutate1 <-
        function(dataframe, ...) {
                col <- list(...)
                
                dataframe <- 
                        dataframe %>%
                        dplyr::mutate(...)

                 dataframe %>%
                         dplyr::select(!!names(col), everything())
        }

