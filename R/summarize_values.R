#' Summarize the counts of values in columns
#' @param ... grouping vars. If missing, groups and summarizes based on all columns.
#' @examples 
#' data("mtcars")
#' summarize_values(mtcars)
#' summarize_values(mtcars, mpg, cyl)
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr enquos
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @export


summarize_values <-
        function(dataframe,
                 ...,
                 desc = TRUE) {
                
                
                if (!missing(...)) {
                        
                                cols <- dplyr::enquos(...)
                                
                                if (desc == FALSE) {
                                        dataframe %>%
                                                dplyr::group_by(!!!cols) %>%
                                                dplyr::summarize(COUNT = n()) %>%
                                                dplyr::arrange(COUNT) %>%
                                                dplyr::ungroup()
                                } else {
                                        dataframe %>%
                                                dplyr::group_by(!!!cols) %>%
                                                dplyr::summarize(COUNT = n()) %>%
                                                dplyr::arrange(dplyr::desc(COUNT)) %>%
                                                dplyr::ungroup()
                                }
                        
                } else {
                        
                        if (desc == FALSE) {
                                dataframe %>% 
                                        dplyr::group_by_at(vars(everything())) %>%
                                        dplyr::summarize(COUNT = n()) %>%
                                        dplyr::arrange(COUNT) %>%
                                        dplyr::ungroup()
                        } else {
                                dataframe %>%
                                        dplyr::group_by_at(vars(everything())) %>%
                                        dplyr::summarize(COUNT = n()) %>%
                                        dplyr::arrange(dplyr::desc(COUNT)) %>%
                                        dplyr::ungroup()
                        }
                        
                }
                
                
        }
