#' Loop a filter over words in a string until there are zero rows
#' This function splits a string based on the provide split argument, and recursively filters a dataframe for those words at a given column. It differs from the filter_by_words_in_str function because it returns a dataframe if the loop ends up evaluating to zero rows.
#' @inheritParams filter_by_words_in_str
#' @importFrom secretary typewrite
#' @export

filter_by_words_in_str <-
        function(string, split, dataframe, col) {
                col <- dplyr::enquo(col)
                Args <- strsplit(string, split = split) %>% unlist()
                
                while (length(Args) > 0) {
                                dataframe2 <-
                                        dataframe %>%
                                        filter_at_grepl(col = !!col, Args[1])
                                
                                
                                if (nrow(dataframe2) == 0) {
                                        secretary::typewrite("Dataframe evaluates to zero rows when filtering for ", Args[1], ". Returning dataframe immediately prior to this filter.")
                                        return(dataframe)
                                } else {
                                        dataframe <- dataframe2
                                        rm(dataframe2)
                                        Args <- Args[-1]
                                }
                }
                secretary::typewrite("All words in the string were filtered for.")
                return(dataframe)
        }