#' Filters most recent values in dataframe based on timestamp or date data
#' @param ... grouping variables
#' @param index_time_col variable that will be used to filter by time or datettime
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate_at
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate ymd
#' @export

filter_most_recent_obs_by_group <-
        function (dataframe, ..., index_time_col, time_format = c("date", "timestamp")) {
                group_by_cols   <- enquos(...)
                index_time_col <- enquo(index_time_col)

                if (time_format == "timestamp") {
                        return(
                        dataframe %>%
                                dplyr::ungroup() %>%
                                dplyr::group_by(!!!group_by_cols) %>%
                                dplyr::mutate_at(vars(!!index_time_col), funs(lubridate::ymd_hms(.))) %>%
                                dplyr::arrange(desc(!!index_time_col)) %>%
                                dplyr::filter(row_number() == 1) %>%
                                dplyr::ungroup() %>%
                                dplyr::mutate_all(as.character)
                        )
                } else if (time_format == "date") {
                        return(
                                dataframe %>%
                                        dplyr::ungroup() %>%
                                        dplyr::group_by(!!!group_by_cols) %>%
                                        dplyr::mutate_at(vars(!!index_time_col), funs(lubridate::ymd(.))) %>%
                                        dplyr::arrange(desc(!!index_time_col)) %>%
                                        dplyr::filter(row_number() == 1) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::mutate_all(as.character)
                        )
                }

        }
