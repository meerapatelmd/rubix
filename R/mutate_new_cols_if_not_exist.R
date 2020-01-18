#' Adds a column if it doesn't exist in the dataframe
#' @param column_name character string of new column name
#' @param value character vector of length 1 of the new value. Defaults to "".
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export

mutate_new_cols_if_not_exist <-
        function(dataframe, column_name, value = "") {
                if (!(column_name %in% colnames(dataframe))) {
                        column_name <- enquo(column_name)

                        x <-
                                dataframe %>%
                                dplyr::mutate(`:=`(!!column_name, value))

                        return(x)
                } else {
                        return(dataframe)
                }

        }
