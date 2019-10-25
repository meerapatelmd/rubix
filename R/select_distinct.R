#' Selects columns and get the distinct values back
#' @import dplyr
#' @export

select_distinct <-
        function(dataframe, ...) {
                vars <- enquos(...)

                dataframe %>%
                        dplyr::select(!!!vars) %>%
                        dplyr::distinct()
        }
