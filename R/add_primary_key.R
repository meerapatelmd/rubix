#' Adds primary key to dataframe in position 1
#' @param pkey_column_name name of pkey column
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @export
add_primary_key <-
        function(dataframe, pkey_column_name) {
                pkey_column_name <- enquo(pkey_column_name)

                dataframe %>%
                        dplyr::mutate(!!pkey_column_name := 1:dplyr::n()) %>%
                        dplyr::select(!!pkey_column_name, dplyr::everything()) %>%
                        call_mr_clean()
        }
