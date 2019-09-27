add_primary_key <-
        function(dataframe, pkey_column_name) {
                pkey_column_name <- enquo(pkey_column_name)

                dataframe %>%
                        dplyr::mutate(!!pkey_column_name := 1:dplyr::n()) %>%
                        dplyr::select(!!pkey_column_name, dplyr::everything()) %>%
                        call_mr_clean()
        }
