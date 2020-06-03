#' Normalize NA values
#' @description convert all "NA" and true NAs to NA
#' @importFrom stringr str_replace_all
#' @importFrom dplyr mutate_all
#' @importFrom dplyr na_if
#' @export

normalize_na <-
        function(.data) {
                .data %>%
                        dplyr::mutate_all(stringr::str_replace_all, "^NA$", "") %>%
                        dplyr::mutate_all(dplyr::na_if, "")
        }