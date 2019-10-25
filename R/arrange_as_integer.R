#' Arrange a column of character data class as an integer
#' @import dplyr
#' @export

arrange_as_integer <-
        function(dataframe, column, desc = FALSE) {
                column <- enquo(column)

                if (desc == FALSE) {
                        dataframe %>%
                                dplyr::arrange(as.integer(!!column))
                } else {
                        dataframe %>%
                                dplyr::arrange(desc(as.integer(!!column)))
                }

        }
