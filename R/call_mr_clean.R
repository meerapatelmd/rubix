#' Mutates all columns of a dataframe to character and trims all white spaces
#' @importFrom dplyr mutate_all
call_mr_clean <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character) %>%
                dplyr::mutate_all(trimws, "both")
}

