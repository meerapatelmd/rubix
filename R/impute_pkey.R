#' Imputes true NA values in given columns with previous value (starting at position 2)
#' @param ... columns to be imputed
#' @importFrom dplyr mutate_at
#' @importFrom caterpillaR carry_forward
#' @export
#'
impute_pkey <-
        function(dataframe, pkey_column) {
                impute_vars <- enquo(pkey_column)

                dataframe %>%
                        dplyr::mutate_at(vars(!!impute_vars), list(~ caterpillaR::carry_forward_and_add_one(.)))
        }
