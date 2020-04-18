#' Arrange dataframe based on nchar
#' Order a dataframe based on the number of characters in the column provided as an argument.
#' @inheritParams call_mr_clean
#' @param nchar_col column where number of characters will determine the order
#' @param desc Should the output be in descending or ascending order?
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export

arrange_by_nchar <-
        function(dataframe, nchar_col, desc = FALSE) {
                
                nchar_col <- dplyr::enquo(nchar_col)

                if (desc == FALSE) {
                        dataframe <-
                                dataframe %>%
                                dplyr::mutate(nchar = nchar(as.character(!!nchar_col))) %>%
                                dplyr::arrange(nchar) %>%
                                dplyr::select(-nchar)

                        return(dataframe)
                } else {
                        dataframe <-
                                dataframe %>%
                                dplyr::mutate(nchar = nchar(as.character(!!nchar_col))) %>%
                                dplyr::arrange(desc(nchar)) %>%
                                dplyr::select(-nchar)

                        return(dataframe)
                }

        }
