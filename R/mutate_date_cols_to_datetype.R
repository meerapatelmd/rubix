#' Mutates date columns to date type
#' @importFrom dplyr mutate_at
#' @export
mutate_date_cols_to_datetype <-
        function(dataframe, ...) {
                date_vars <- enquos(...)
                
                dataframe %>%
                        mutate_at(vars(!!!date_vars), funs(lubridate::ymd(.)))
        }