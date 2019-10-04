#' Get the sum of the rows base on the designated columns
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @export

mutate_rowsum <-
        function(dataframe, ...) {
                sum_vars <- enquos(...)
                
                sums <- vector()
                for (i in 1:nrow(dataframe)) {
                        x <- dataframe %>%
                                dplyr::select(!!!sum_vars)
                        sums[i] <- sum(as.numeric(x[i,]))
                }
                
                dataframe %>%
                        dplyr::mutate(rowsum = sums)
        }
