#' Loop over searching for words in a string in a dataframe
#' @importFrom secretary typewrite
#' @export

filter_by_words_in_str <-
        function(string, split, dataframe, col) {
                col <- dplyr::enquo(col)
                Args <- strsplit(string, split = split) %>% unlist()
                
                while (length(Args) > 0) {
                        if (nrow(dataframe) > 0) {
                                dataframe <-
                                        dataframe %>%
                                        filter_at_grepl(col = !!col, Args[1])
                                
                                Args <- Args[-1]
                        } else {
                                secretary::typewrite("Stopped at ", paste(Args, collapse = " "))
                                return(dataframe)
                        }
                }
                return(dataframe)
        }