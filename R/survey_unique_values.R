#' Get all the unique values in each column printed out as a list of vectors
#' @param dataframe dataframe with target columns to iterate over
#' @param alphabetical_order defaults to TRUE where columns will be returned in alphabetical order.
#' @import dplyr
#' @importFrom purrr set_names
#' @export


survey_unique_values <-
        function(dataframe, alphabetical_order = TRUE) {
                output <-
                        lapply(1:ncol(dataframe),
                               function(x) dataframe %>%
                                       dplyr::select(x) %>%
                                       dplyr::distinct() %>%
                                       unlist() %>%
                                       unname()) %>%
                        purrr::set_names(colnames(dataframe))
                
                if (alphabetical_order == TRUE) {
                        output <- output[names(output)[order(names(output))]]
                        return(output)
                } else {
                        return(output)
                }
        }