#' @title 
#' Sort a Dataframe 
#' 
#' @export
#' @rdname arrange_sort

arrange_sort <- 
        function(data,
                 col,
                 levels) {
                
                data %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), 
                                         ~ factor(., levels = levels)
                                         ) %>%
                        dplyr::arrange_at(dplyr::vars({{ col }})) %>%
                        dplyr::mutate_at(dplyr::vars({{ col }}), 
                                         ~ as.character(.))
        }