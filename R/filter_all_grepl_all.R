#' @title 
#' Filter all the variables for all matches to the grepl phrase
#' @description 
#' This function will return rows that have all TRUE findings of the indicated grepl phrase
#' @param dataframe input dataframe
#' @param grepl_phrase phrase that is being filtered for
#' @param evaluates_to whether the filter is for a grepl evaluation of TRUE or FALSE. Defaults to TRUE.
#' @param ignore.case boolean the ignore.case argument of grepl function
#' @export
#' @seealso 
#'  \code{\link[dplyr]{filter_all}}
#' @rdname filter_all_grepl_all
#' @export 
#' @importFrom dplyr filter_all %>%

filter_all_grepl_all <-
        function(dataframe,
                 grepl_phrase,
                 evaluates_to = TRUE,
                 ignore.case = TRUE) {
                
                
                if (evaluates_to == TRUE) {
                        return(
                                dataframe %>%
                                        dplyr::filter_all(all_vars(grepl(grepl_phrase, ., ignore.case = ignore.case) == TRUE))
                        )
                } else {
                        return(
                                dataframe %>% 
                                dplyr::filter_all(all_vars(grepl(grepl_phrase, ., ignore.case = ignore.case) == FALSE))
                        )
                }
        }