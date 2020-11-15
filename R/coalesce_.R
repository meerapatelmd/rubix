#' Coalesce 1+ Fields to an Existing Field
#' @description 
#' Coalesce a set of fields to an existing column `col` in the order in which the arguments are supplied with `col` leading.
#' 
#' @inheritParams wrapper_args
#' @param ... Other columns in addition to `col` that will be coalesced in the correct order of precedence. 
#' @param remove Should columns supplied in `...` be removed in the output?
#' @example inst/examples/coalesce_.R
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{coalesce}},\code{\link[dplyr]{select_all}}
#' @rdname coalesce_at
#' @family coalesce functions
#' @export
#' @importFrom dplyr enquos select mutate_at vars mutate coalesce select_at



coalesce_at <- 
        function(data,
                 col,
                 ...,
                 remove = TRUE) {

                cols <- dplyr::enquos(...)

                
                if (remove) {
                        
                        rm_cols_vector <- 
                                data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()
                 
                        data %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}, !!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ col }} := dplyr::coalesce({{ col }}, !!!cols)) %>%
                                dplyr::select_at(dplyr::vars(!all_of(rm_cols_vector)))
                        
                        
                } else {
                        
                        data %>%
                                dplyr::mutate_at(dplyr::vars({{ col }}, !!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ col }} := dplyr::coalesce({{ col}},
                                                                              !!!cols))
                }


        }





#' @title
#' Coalesce 1+ Fields to a New Field
#' 
#' @description 
#' (Deprecated) This is 1 of 2 coalesce functions. This one takes fields that are not the same as the proposed new column name. For example, a dataframe with fields "A", "B", and "C" can be coalesced into a field "D". If all the values need to be coalesced into an existing field such as "A", "B", and "C" to "A", use coalesce_at function instead.
#' 
#' @inheritParams coalesce_at
#' @example inst/examples/coalesce_.R
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{coalesce}}
#'  
#' @rdname coalesce_to
#' @family coalesce functions
#' @export 
#' @importFrom dplyr vars enquos select mutate mutate_at coalesce select_at

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
                                dplyr::mutate_at(dplyr::vars(!!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ col }} := dplyr::coalesce(!!!cols)) %>%
                                dplyr::select_at(dplyr::vars(!all_of(rm_cols_vector)))


                } else {

                        data %>%
                                dplyr::mutate({{ col }} := "") %>%
                                dplyr::mutate_at(dplyr::vars(!!!cols),
                                                 as.character) %>%
                                dplyr::mutate({{ col }} := dplyr::coalesce(!!!cols))
                }
                        
        }





