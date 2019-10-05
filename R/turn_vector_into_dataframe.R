#' Converts vector to a dataframe with names as column names
#' @param vector vector of any length with names
#' @export

turn_vector_into_dataframe <-
        function(vector) {
                return(data.frame(t(vector)))
        }