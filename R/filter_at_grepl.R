#' Filter at a variable that contains or doesn't contain a phrase
#' @importFrom dplyr enquo
#' @importFrom dplyr filter_at
#' @importFrom dplyr vars
#' @importFrom dplyr any_vars
#' @param dataframe input dataframe
#' @param col column to filter
#' @param grepl_phrase phrase that is being filtered for
#' @param evaluates_to whether the filter is for a grepl evaluation of TRUE or FALSE. Defaults to TRUE.
#' @param ignore.case boolean the ignore.case argument of grepl function
#' @export

filter_at_grepl <-
        function(dataframe,
                 col,
                 grepl_phrase,
                 evaluates_to = TRUE,
                 ignore.case = TRUE) {
                
                col <- dplyr::enquo(col)
                
                if (evaluates_to == TRUE) {
                        return(
                                dataframe %>%
                                        dplyr::filter_at(dplyr::vars(!!col),
                                                         dplyr::any_vars(grepl(grepl_phrase, ., ignore.case = ignore.case) == TRUE))
                        )
                } else {
                        return(
                                dataframe %>%
                                        dplyr::filter_at(dplyr::vars(!!col),
                                                         dplyr::any_vars(grepl(grepl_phrase, ., ignore.case = ignore.case) == FALSE))
                        )
                }
        }