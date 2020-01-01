#' Get all distinct values for a particular column
#' @import dplyr
#' @export

select_distinct_column <-
        function(dataframe, col) {
                col <- enquo(col)
                
                dataframe %>%
                        dplyr::select(!!col) %>%
                        dplyr::distinct()
        }