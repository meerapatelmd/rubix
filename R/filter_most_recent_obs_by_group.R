#' Filters most recent values in dataframe
#' @param group_by_col grouping variable
#' @param index_date_col variable that will be used to filter by time
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate_at
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom lubridate ymd_hms
#' @export

filter_most_recent_obs_by_group <-
        function (dataframe, group_by_col, index_date_col) {
                group_by_col   <- enquo(group_by_col)
                index_date_col <- enquo(index_date_col)

                dataframe %>%
                        dplyr::ungroup() %>%
                        dplyr::group_by(!!group_by_col) %>%
                        dplyr::mutate_at(vars(!!index_date_col), funs(lubridate::ymd_hms(.))) %>%
                        dplyr::arrange(desc(!!index_date_col)) %>%
                        dplyr::filter(row_number() == 1) %>%
                        dplyr::ungroup()
        }
