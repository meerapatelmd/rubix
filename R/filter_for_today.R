#' Filters a dataframe for today's date in a timestamp column
#' @param dataframe dataframe with timestamp column
#' @param timestamp_col column where timestamp has date and/or datetime data that is in the format YYYY-mm-dd
#' @return dataframe filtered for today's date
#' @import dplyr
#' @export

filter_for_today <-
        function(dataframe, timestamp_col) {
                timestamp_col <- enquo(timestamp_col)

                today <- as.character(Sys.Date())

                return(
                        dataframe %>%
                                dplyr::filter_at(vars(!!timestamp_col), dplyr::all_vars(grepl(today, .) == TRUE))
                )

        }
