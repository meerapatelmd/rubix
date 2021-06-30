#' @title 
#' Rename Fields with a Character Vector
#' @description 
#' Rename the fields of a dataframe with 
#' a vector of new field names.
#' @param new_colnames          Character vector of new names with same length as ncols of data argument.
#' @seealso 
#'  \code{\link[dplyr]{rename_at}}
#' @rdname rename_fields
#' @export 
#' @importFrom dplyr rename_at



rename_fields <- 
        function(data, 
                 new_colnames) {
                
                data %>%
                        dplyr::rename_at(.,
                                         names(data),
                                         funs(c(new_colnames)))
                
                
        }