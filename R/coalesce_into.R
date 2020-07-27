#' Coalesce 1+ Fields to a New Field
#' @description This is 1 of 2 coalesce functions. This one takes fields that are not the same as the proposed new column name. For example, a dataframe with fields "A", "B", and "C" can be coalesced into a field "D". If all the values need to be coalesced into an existing field such as "A", "B", and "C" to "A", use coalesce_at function instead.
#' @import dplyr
#' @export


coalesce_into <- 
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