#' Filters a dataframe for a date in YYYY-mm-dd format in a timestamp column
#' @param .data .data with timestamp column
#' @param timestamp_col column where timestamp has date and/or datetime data that is in the format YYYY-mm-dd
#' @param date date as character string of length one in format YYYY-mm-dd
#' @return dataframe filtered for the date
#' @seealso 
#'  \code{\link[dplyr]{filter_all}},\code{\link[dplyr]{all_vars}}
#' @rdname filter_for_date
#' @export 
#' @importFrom dplyr filter_at all_vars %>%

filter_for_date <-
        function(.data, timestamp_col, date) {
                timestamp_col <- enquo(timestamp_col)

                return(
                        .data %>%
                                dplyr::filter_at(vars(!!timestamp_col), dplyr::all_vars(grepl(date, .) == TRUE))
                )

        }
