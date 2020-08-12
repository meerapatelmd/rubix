#' @title
#' Selects columns and get the distinct values back
#' @seealso 
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}}
#' @rdname select_distinct
#' @export 
#' @importFrom dplyr select distinct %>%

select_distinct <-
        function(dataframe, ...) {
                vars <- enquos(...)

                dataframe %>%
                        dplyr::select(!!!vars) %>%
                        dplyr::distinct()
        }
