#' @title 
#' Filter at a variable that contains or doesn't contain a phrase
#' @param dataframe input dataframe
#' @param col column to filter
#' @param grepl_phrase phrase that is being filtered for
#' @param evaluates_to whether the filter is for a grepl evaluation of TRUE or FALSE. Defaults to TRUE.
#' @param ignore.case boolean the ignore.case argument of grepl function
#' @seealso 
#'  \code{\link[dplyr]{filter_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{all_vars}}
#' @rdname filter_all_grepl
#' @export 
#' @importFrom dplyr filter_all vars any_vars filter_at %>%

filter_all_grepl <-
        function(dataframe,
                 grepl_phrase,
                 evaluates_to = TRUE,
                 ignore.case = TRUE) {
                
                
                if (evaluates_to == TRUE) {
                        return(
                                dataframe %>%
                                        dplyr::filter_all(dplyr::vars(!!col),
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