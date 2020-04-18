#' Loop a filter over words in a string
#' This function splits a string based on the provide split argument, and recursively filters a dataframe for those words at a given column.
#' @param string
#' @param split
#' @param dataframe
#' @param col
#' @importFrom secretary typewrite
#' @importFrom dplyr enquo
#' @export

filter_by_words_in_str <-
        function(string, split, dataframe, col) {
                
                col <- dplyr::enquo(col)
                
                Args <- strsplit(string, split = split) %>% unlist()
                
                while (length(Args) > 0) {
                                dataframe <-
                                        dataframe %>%
                                        filter_at_grepl(col = !!col, Args[1])
                                
                                Args <- Args[-1]
                }
                return(dataframe)
        }