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





#' @title
#' Coalesce 1+ Fields to a New Field
#' @description 
#' (Deprecated) This is 1 of 2 coalesce functions. This one takes fields that are not the same as the proposed new column name. For example, a dataframe with fields "A", "B", and "C" can be coalesced into a field "D". If all the values need to be coalesced into an existing field such as "A", "B", and "C" to "A", use coalesce_at function instead.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{coalesce}}
#' @rdname coalesce_into
#' @export 
#' @importFrom dplyr enquos enquo select mutate coalesce %>%


coalesce_to <- 
        function(data,
                 col,
                 ...,
                 remove = FALSE) {
                
                
                
                if (col_exists(data = data, 
                               col = {{ col }})) {
                        
                        stop("`col` exists in data. Try another `col` or `coalesce_at()`")
                        
                }

                
                cols <- dplyr::enquos(...)

                if (remove) {

                        rm_cols_vector <-
                                data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()

                        data %>%
                                dplyr::mutate({{ col }} := "") %>%
                                dplyr::mutate_at(vars(!!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ col }} := dplyr::coalesce(!!!cols)) %>%
                                dplyr::select_at(vars(!all_of(rm_cols_vector)))


                } else {

                        data %>%
                                dplyr::mutate({{ col }} := "") %>%
                                dplyr::mutate_at(vars(!!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ col }} := dplyr::coalesce(!!!cols))
                }
                        
        }





