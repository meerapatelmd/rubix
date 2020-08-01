#' Arrange by Column as an Integer
#' @description Perform an arrange function call on a dataframe with the values of the target column as an integer class.
#' @import dplyr
#' @export

arrange_as_integer <-
        function(.data, column, desc = FALSE) {
                
                column <- dplyr::enquo(column)
                

                if (desc == FALSE) {
                        .data %>%
                                dplyr::arrange(as.integer(!!column))
                } else {
                        .data %>%
                                dplyr::arrange(desc(as.integer(!!column)))
                }

        }
