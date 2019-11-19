#' Filters a dataframe for a date in YYYY-mm-dd format in a timestamp column
#' @param dataframe dataframe with timestamp column
#' @param timestamp_col column where timestamp has date and/or datetime data that is in the format YYYY-mm-dd
#' @param date date as character string of length one in format YYYY-mm-dd
#' @return dataframe filtered for the date
#' @import dplyr
#' @export

filter_for_date <-
        function(dataframe, timestamp_col, date) {
                timestamp_col <- enquo(timestamp_col)

                return(
                        dataframe %>%
                                dplyr::filter_at(vars(!!timestamp_col), dplyr::all_vars(grepl(date, .) == TRUE))
                )

        }
