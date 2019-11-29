#' Imputes true NA values in given columns with previous value (starting at position 2)
#' @importFrom dplyr mutate_all
#' @importFrom caterpillaR carry_forward
#' @export
#'
impute_all_with_carry_forward <-
        function(dataframe) {
                dataframe %>%
                        dplyr::mutate_all(caterpillaR::carry_forward)
        }
