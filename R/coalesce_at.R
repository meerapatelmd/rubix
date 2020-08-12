#' Coalesce 1+ Fields to an Existing Field
#' @description This is 2 of 2 coalesce functions. 
#' @param at_var character vector of length 1 of the column that the coalesce function will be combine to
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}}
#' @rdname coalesce_at
#' @export 
#' @importFrom dplyr enquos mutate_at select mutate %>%


coalesce_at <- 
        function(.data,
                 at_var,
                 ...,
                 remove = FALSE) {
                
                at_var <- ensym(at_var)
                at_var <- enquo(at_var)

                cols <- dplyr::enquos(...)

                .data <-
                        .data %>%
                        dplyr::mutate_at(vars(c(!!at_var, !!!cols)),
                                         as.character)


                if (remove) {
                        remove_cols <-
                                .data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()
                        
                        .data %>%
                                dplyr::mutate(!!at_var := coalesce(!!at_var, !!!cols)) %>%
                                dplyr::select(-all_of(remove_cols))
                } else {

                        .data %>%
                                dplyr::mutate(!!at_var := coalesce(!!at_var, !!!cols))
                }

        }