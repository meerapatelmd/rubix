#' Coalesce 1+ Fields to an Existing Field
#' @description This is 2 of 2 coalesce functions. 
#' @param at_var character vector of length 1 of the column that the coalesce function will be combine to
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}}
#' @rdname coalesce_at
#' @export 
#' @importFrom dplyr enquos mutate_at select mutate %>%


coalesce_at <- 
        function(data,
                 at_var,
                 ...,
                 remove = TRUE) {

                cols <- dplyr::enquos(...)

                
                if (remove) {
                        
                        rm_cols_vector <- 
                                data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()
                 
                        data %>%
                                dplyr::mutate_at(vars({{ at_var }}, !!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ at_var }} := dplyr::coalesce({{ at_var }}, !!!cols)) %>%
                                dplyr::select_at(vars(!all_of(rm_cols_vector)))
                        
                        
                } else {
                        
                        data %>%
                                dplyr::mutate_at(vars({{ at_var }}, !!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ at_var }} := dplyr::coalesce({{ at_var}},
                                                                              !!!cols))
                }


        }
