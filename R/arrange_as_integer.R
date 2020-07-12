#' Arrange a column any data class as an integer
#' @import dplyr
#' @export

arrange_as_integer <-
        function(.data, column, desc = FALSE) {
                column <- enquo(column)
                

                if (desc == FALSE) {
                        .data %>%
                                dplyr::arrange(as.integer(!!column))
                } else {
                        .data %>%
                                dplyr::arrange(desc(as.integer(!!column)))
                }

        }
