#' Filter at a variable that contains a phrase
#' @import dplyr
#' @export

filter_at_grepl <-
        function(dataframe,
                 col,
                 grepl_phrase) {
                
                col <- enquo(col)
                
                return(
                        dataframe %>%
                                dplyr::filter_at(vars(!!col),
                                                 any_vars(grepl(grepl_phrase, ., ignore.case = TRUE) == TRUE))
                )
        }