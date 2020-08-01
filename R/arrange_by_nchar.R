#' Arrange by Character Number 
#' @description  Order a dataframe based on the number of characters in the column provided as an argument.
#' @param nchar_col column where number of characters will determine the order
#' @param desc Should the output be in descending or ascending order?
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @importFrom magrittr %>% 
#' @export

arrange_by_nchar <-
        function(.data, nchar_col, desc = FALSE) {
                
                nchar_col <- dplyr::enquo(nchar_col)

                if (desc == FALSE) {
                                .data %>%
                                dplyr::mutate(nchar = nchar(as.character(!!nchar_col))) %>%
                                dplyr::arrange(nchar) %>%
                                dplyr::select(-nchar)
                } else {
                                .data %>%
                                dplyr::mutate(nchar = nchar(as.character(!!nchar_col))) %>%
                                dplyr::arrange(desc(nchar)) %>%
                                dplyr::select(-nchar)
                }

        }
