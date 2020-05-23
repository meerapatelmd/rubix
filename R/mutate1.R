#' Mutate a column to position 1
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export


mutate1 <-
        function(dataframe, ...) {
                col <- list(...)
                for (i in 1:length(col)) {
                        dataframe <-
                                dataframe %>%
                                dplyr::mutate(`:=`(!!names(col)[i], col[[i]])) 
                                
                                
                }

                 dataframe %>%
                         dplyr::select(!!names(col), everything())
        }

