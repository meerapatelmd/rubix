#' Coalesce 1+ Fields to a New Field
#' @import dplyr
#' @export


coalesce_to <- 
        function(.data,
                 into,
                 ...,
                 remove = FALSE) {
                
                cols <- dplyr::enquos(...)
                into <- dplyr::enquo(into)
                
                if (remove) {
                        remove_cols <- 
                                .data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()
                        
                        .data %>% 
                                dplyr::mutate(!!into := dplyr::coalesce(!!!cols)) %>%
                                dplyr::select(-all_of(remove_cols))
                } else {
                        
                        .data %>% 
                                dplyr::mutate(!!into := dplyr::coalesce(!!!cols))
                }
                        
        }