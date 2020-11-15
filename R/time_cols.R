#' @importFrom dplyr mutate select everything 
#' @export


datetime_to_col <- 
        function(data, var = "datetime") {
                
                data %>%
                        dplyr::mutate({{ var }} := Sys.time()) %>%
                        dplyr::select({{ var }},
                                      dplyr::everything()
                                      )
                
                
        }

#' @importFrom dplyr mutate select everything 
#' @export


date_to_col <- 
        function(data, var = "date") {
                
                data %>%
                        dplyr::mutate({{ var }} := Sys.Date()) %>%
                        dplyr::select({{ var }},
                                      dplyr::everything()
                        )
                
                
        }