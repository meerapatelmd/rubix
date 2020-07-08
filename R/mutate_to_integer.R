#' Mutate cols to integer data type
#' @import dplyr
#' @export

mutate_to_integer <- 
        function(.data, ...) {
                
                cols <- enquos(...)
                
                .data %>%
                        dplyr::mutate_at(vars(!!!cols), as.integer)
                
                
        }