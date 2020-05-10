#' Summarize groups by count
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr enquos
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @export


summarize_grouped_n <-
        function(dataframe,
                 ...,
                 desc = FALSE) {
                
                cols <- dplyr::enquos(...)
                
                if (desc == FALSE) {
                        dataframe %>%
                                dplyr::group_by(!!!cols) %>%
                                dplyr::summarize(n = n()) %>%
                                dplyr::arrange(n) %>%
                                dplyr::ungroup()
                } else {
                        dataframe %>%
                                dplyr::group_by(!!!cols) %>%
                                dplyr::summarize(n = n()) %>%
                                dplyr::arrange(dplyr::desc(n)) %>%
                                dplyr::ungroup()
                }
                
        }