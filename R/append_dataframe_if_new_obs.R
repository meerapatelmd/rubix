#' Adds an observation with tinmestamp to a column by given id where column names match environment variables
#' @param id_variable name of variable and object name that designates a character vector of ids
#' @param new_obs_variable name of variable and object name that provides the input
#' @param timestamp_col_name name of the timestamp column that will be timestampped if the observation needs to be added
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @export
#'

append_dataframe_if_new_obs <-
        function(dataframe, id_variable, new_obs_variable, timestamp_col_name) {
                id_variable_value      <- get(id_variable, envir = globalenv())
                new_obs_variable_value <- get(new_obs_variable, envir = globalenv())

                id_variable <- enquo(id_variable)

                new_obs_variable <- enquo(new_obs_variable)

                timestamp_col_name <- enquo(timestamp_col_name)

                dataframe_name <- deparse(substitute(dataframe))

                df_0 <- dataframe %>%
                        somersaulteR::filter_for_vector(!!id_variable, inclusion_vector = id_variable_value)

                df_1 <-
                        df_0 %>%
                        somersaulteR::filter_for_vector(!!new_obs_variable, inclusion_vector = new_obs_variable_value)

                if (nrow(df_1) == 0) {
                        x <- df_0 %>%
                                dplyr::filter(row_number() == 1) %>%
                                dplyr::mutate(!!timestamp_col_name := mirroR::get_timestamp()) %>%
                                dplyr::mutate(!!new_obs_variable := new_obs_variable_value) %>%
                                somersaulteR::call_mr_clean()

                        x <- dplyr::bind_rows(dataframe %>%
                                                      somersaulteR::call_mr_clean, x)

                        return(x)
                } else {
                        return(dataframe)
                }


        }
