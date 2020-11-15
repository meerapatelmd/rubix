#' @title 
#' Filter all the variables for all matches to the grepl phrase
#' @description 
#' This function will return rows that have all TRUE findings of the indicated grepl phrase
#' @param data input dataframe
#' @param grepl_phrase phrase that is being filtered for
#' @param evaluates_to whether the filter is for a grepl evaluation of TRUE or FALSE. Defaults to TRUE.
#' @inheritParams base::grepl
#' @export
#' @seealso 
#'  \code{\link[dplyr]{filter_all}}
#' @rdname filter_all_grepl_all
#' @export 
#' @importFrom dplyr filter_all %>%

filter_all_grepl_all <-
        function(data,
                 grepl_phrase,
                 evaluates_to = TRUE,
                 ignore.case = FALSE,
                 perl = FALSE,
                 fixed = FALSE,
                 useBytes = FALSE) {
                
                
                if (evaluates_to == TRUE) {
                
                        data %>%
                                dplyr::filter_all(dplyr::all_vars(grepl(pattern = grepl_phrase,
                                                                         x = .,
                                                                         ignore.case = ignore.case,
                                                                         perl = perl,
                                                                         fixed = fixed,
                                                                         useBytes = useBytes) == TRUE))
                        
                } else {
                        
                        data %>%
                                dplyr::filter_all(dplyr::all_vars(grepl(pattern = grepl_phrase,
                                                                        x = .,
                                                                        ignore.case = ignore.case,
                                                                        perl = perl,
                                                                        fixed = fixed,
                                                                        useBytes = useBytes) == FALSE))
                        
                }
        }





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
        function(data,
                 grepl_phrase,
                 evaluates_to = TRUE,
                 ignore.case = TRUE) {
                
                
                if (evaluates_to == TRUE) {
                        
                        data %>%
                                dplyr::filter_all(dplyr::any_vars(grepl(pattern = grepl_phrase,
                                                                        x = .,
                                                                        ignore.case = ignore.case,
                                                                        perl = perl,
                                                                        fixed = fixed,
                                                                        useBytes = useBytes) == TRUE))
                        
                } else {
                        
                        data %>%
                                dplyr::filter_all(dplyr::any_vars(grepl(pattern = grepl_phrase,
                                                                        x = .,
                                                                        ignore.case = ignore.case,
                                                                        perl = perl,
                                                                        fixed = fixed,
                                                                        useBytes = useBytes) == FALSE))
                        
                }
                
        }


#' @title 
#' Filter at a variable that contains or doesn't contain a phrase
#' @param dataframe input dataframe
#' @param col column to filter
#' @param grepl_phrase phrase that is being filtered for
#' @param evaluates_to whether the filter is for a grepl evaluation of TRUE or FALSE. Defaults to TRUE.
#' @param ignore.case boolean the ignore.case argument of grepl function
#' @seealso 
#'  \code{\link[dplyr]{filter_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{all_vars}}
#' @rdname filter_all_grepl_any
#' @export 
#' @importFrom dplyr filter_all vars any_vars filter_at %>%

filter_all_grepl_any <-
        function(data,
                 grepl_phrase,
                 evaluates_to = TRUE,
                 ignore.case = TRUE) {
                
                
                if (evaluates_to == TRUE) {
                        
                        data %>%
                                dplyr::filter_all(dplyr::any_vars(grepl(pattern = grepl_phrase,
                                                                        x = .,
                                                                        ignore.case = ignore.case,
                                                                        perl = perl,
                                                                        fixed = fixed,
                                                                        useBytes = useBytes) == TRUE))
                        
                } else {
                        
                        data %>%
                                dplyr::filter_all(dplyr::any_vars(grepl(pattern = grepl_phrase,
                                                                        x = .,
                                                                        ignore.case = ignore.case,
                                                                        perl = perl,
                                                                        fixed = fixed,
                                                                        useBytes = useBytes) == FALSE))
                        
                }
                
        }


