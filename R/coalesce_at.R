#' Coalesce 1+ Fields to an Existing Field
#' @description This is 2 of 2 coalesce functions. 
#' @param at_var character vector of length 1 of the column that the coalesce function will be combine to
#' @import dplyr
#' @export


coalesce_at <- 
        function(.data,
                 at_var,
                 ...,
                 remove = FALSE) {
                
                cols <- dplyr::enquos(...)

                if (remove) {
                        remove_cols <- 
                                .data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()
                        remove_cols <- remove_cols[!(remove_cols %in% at_var)]
                        
                        .data %>% 
                                dplyr::mutate(!!at_var := coalesce(!!!cols)) %>%
                                dplyr::select(-all_of(remove_cols))
                } else {
                        
                        .data %>% 
                                dplyr::mutate(!!at_var := coalesce(!!!cols)) 
                }
                        
        }