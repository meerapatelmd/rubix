#' Converts vector to a dataframe with names as column names
#' @param named_vector vector of any length with names
#' @import dplyr
#' @export

turn_vector_into_dataframe <-
        function(vector) {
                return(dplyr::tibble(t(vector)))
        }
