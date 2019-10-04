#' Selects columns called upon without prefix that matches dataframe name
#' @param dataframe dataframe where the R object it belongs to has the same name as the prefix of the columns
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom rlang as_string
#' @export

special_select <-
        function(dataframe, ...) {
                
                dataframe_name <- stringr::str_remove(deparse(substitute(dataframe)), "[$]{1}.*$")
                
                args <- ensyms(...)
                args <- paste(dataframe_name, purrr::map(args, rlang::as_string), sep = "_")
                
                dataframe %>%
                        select(args)
                
        }
