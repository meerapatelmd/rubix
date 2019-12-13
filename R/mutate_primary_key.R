#' Adds primary key to dataframe in position 1
#' @param pkey_column_name name of pkey column
#' @param starting_number optional starting number for the primary key
#' @param width_left_pad_with_zero integer of the number of leading zeros desired. NULL by default.
#' @param prefix prefix desired with starting_number + left padding with zeros (if not NULL)
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @export

mutate_primary_key <-
        function(dataframe,
                 pkey_column_name,
                 starting_number = NULL,
                 width_left_pad_with_zero = NULL,
                 prefix = NULL) {

                pkey_column_name <- enquo(pkey_column_name)

                if (is.null(starting_number)) {
                        x <-
                        dataframe %>%
                                dplyr::mutate(!!pkey_column_name := 1:dplyr::n()) %>%
                                dplyr::select(!!pkey_column_name, dplyr::everything())

                        if (!is.null(width_left_pad_with_zero)) {
                                x <- x %>%
                                        dplyr::mutate(!!pkey_column_name := stringr::str_pad(!!pkey_column_name, width = width_left_pad_with_zero, side = "left", pad = "0"))
                        } else {
                                x <- x
                        }
                } else {
                        if (is.null(prefix)) {
                                starting_number <- as.integer(starting_number)
                                row_num <- nrow(dataframe)
                                pkey_values <- starting_number:((starting_number+row_num)-1)

                                x <-
                                        dataframe %>%
                                        dplyr::mutate(!!pkey_column_name := pkey_values) %>%
                                        dplyr::select(!!pkey_column_name, dplyr::everything())

                                print("success")

                                if (!is.null(width_left_pad_with_zero)) {
                                        x <- x %>%
                                                dplyr::mutate_at(vars(!!pkey_column_name), stringr::str_pad, width = width_left_pad_with_zero, side = "left", pad = "0")
                                } else {
                                        x <- x
                                }
                        } else {
                                starting_number <- as.integer(starting_number)
                                row_num <- nrow(dataframe)
                                pkey_values <- starting_number:((starting_number+row_num)-1)

                                x <-
                                        dataframe %>%
                                        dplyr::mutate(!!pkey_column_name := pkey_values) %>%
                                        dplyr::select(!!pkey_column_name, dplyr::everything())

                                if (!is.null(width_left_pad_with_zero)) {
                                        x <- x %>%
                                                dplyr::mutate_at(vars(!!pkey_column_name), stringr::str_pad, width = width_left_pad_with_zero, side = "left", pad = "0")
                                } else {
                                        x <- x
                                }


                                x <-
                                        x %>%
                                        dplyr::mutate(!!pkey_column_name :=
                                                              paste0(prefix,
                                                                     x %>%
                                                                             dplyr::select(!!pkey_column_name) %>%
                                                                             unlist()
                                                              ))

                        }

                }
                return(x)

        }
