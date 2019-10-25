#' Adds primary key to dataframe in position 1
#' @param pkey_column_name name of pkey column
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @export

add_primary_key <-
        function(dataframe, pkey_column_name, starting_number = NULL, prefix = NULL) {
                pkey_column_name <- enquo(pkey_column_name)

                if (is.null(starting_number)) {
                        x <-
                        dataframe %>%
                                dplyr::mutate(!!pkey_column_name := 1:dplyr::n()) %>%
                                dplyr::select(!!pkey_column_name, dplyr::everything())
                } else {
                        if (is.null(prefix)) {
                                starting_number <- as.integer(starting_number)
                                row_num <- nrow(dataframe)
                                pkey_values <- vector()
                                for (i in 1:row_num) {
                                        pkey_values[i] <- starting_number + i
                                }

                                x <-
                                        dataframe %>%
                                        dplyr::mutate(!!pkey_column_name := pkey_values) %>%
                                        dplyr::select(!!pkey_column_name, dplyr::everything())
                        } else {
                                starting_number <- as.integer(starting_number)
                                row_num <- nrow(dataframe)
                                pkey_values <- vector()
                                for (i in 1:row_num) {
                                        pkey_values[i] <- starting_number + i
                                }
                                pkey_values <- paste0(prefix, pkey_values)

                                x <-
                                        dataframe %>%
                                        dplyr::mutate(!!pkey_column_name := pkey_values) %>%
                                        dplyr::select(!!pkey_column_name, dplyr::everything())
                        }

                }
                return(x)

        }
