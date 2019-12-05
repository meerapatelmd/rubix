#' Arrange dataframe based on nchar of a given column
#' @import dplyr
#' @export

arrange_by_nchar <-
        function(dataframe, nchar_col, desc = FALSE) {
                nchar_col <- enquo(nchar_col)

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
