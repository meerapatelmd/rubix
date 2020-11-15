#' Filter a column for a set of values
#' @description This is a shortcut to using the dplyr::filter({col} %in% {vector}) function along with its inverse.
#' @param cols column targeted for filtering
#' @param vector a vector of values to be included
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at
#' @export

filter_for_any <-
        function(data, 
                 ..., 
                 vector, 
                 invert = FALSE) {
                
                cols <- dplyr::enquos(...)
                
                if (invert) {
                        
                        data %>%
                                dplyr::filter_at(vars(!!!cols), dplyr::any_vars(!(. %in% vector)))
                        
                        
                } else {

                        data %>%
                                dplyr::filter_at(vars(!!!cols), dplyr::any_vars(. %in% vector))
                }
                
        }



#' Filter a column for a set of values
#' @description This is a shortcut to using the dplyr::filter({col} %in% {vector}) function along with its inverse.
#' @param cols column targeted for filtering
#' @param vector a vector of values to be included
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at
#' @export

filter_for_all <-
        function(data, 
                 ..., 
                 vector, 
                 invert = FALSE) {
                
                cols <- dplyr::enquos(...)
                
                if (invert) {
                        
                        data %>%
                                dplyr::filter_at(vars(!!!cols), dplyr::all_vars(!(. %in% vector)))
                        
                        
                } else {
                        
                        data %>%
                                dplyr::filter_at(vars(!!!cols), dplyr::all_vars(. %in% vector))
                }
                
        }


#' Filter a column for a set of values
#' @description This is a shortcut to using the dplyr::filter({col} %in% {vector}) function along with its inverse.
#' @param cols column targeted for filtering
#' @param vector a vector of values to be included
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at
#' @export

filter_for <-
        function(data, 
                 col, 
                 vector, 
                 invert = FALSE) {
                

                if (invert) {
                        
                        data %>%
                                dplyr::filter_at(dplyr::vars({{ col }}), dplyr::all_vars(!(. %in% vector)))
                        
                        
                } else {
                        
                        data %>%
                                dplyr::filter_at(dplyr::vars({{ col }}), dplyr::all_vars(. %in% vector))
                }
                
        }
