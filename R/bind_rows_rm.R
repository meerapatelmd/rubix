#' Bind dataframes together and remove from the env
#' @importFrom dplyr bind_rows
#' @export

bind_rows_rm <-
        function(..., .id = NULL) {
                output <- dplyr::bind_rows(..., .id = .id)
                
                Args <- list(...)
                Args %>%
                        purrr::map(rm, envir = globalenv())
                return(output)
        }

