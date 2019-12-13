#' Substitute all true NA values in a dataframe as blank
#' @importFrom dplyr mutate_all
#' @export
mutate_all_na_to_blank <-
        function(dataframe, include_na_as_string = TRUE) {
                x <- dataframe
                x[is.na(x)] <- ""

                if (include_na_as_string == TRUE) {
                        x <- x %>%
                                dplyr::mutate_all(str_replace_all, "NA", "")
                } else {
                        x <- x
                }
                return(x)
        }
