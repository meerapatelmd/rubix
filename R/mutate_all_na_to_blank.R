
#' @export
#'
mutate_all_na_to_blank <-
        function(dataframe) {
                x <- dataframe
                x[is.na(x)] <- ""
                return(x)
        }
