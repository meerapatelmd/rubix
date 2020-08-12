#' @title General Dataframe Cleanup
#' @description
#' Convert all columns to character class and trim all left and right white spaces.
#' @param dataframe input dataframe
#' @importFrom dplyr mutate_all
#' @importFrom dplyr %>% 
#' @export

call_mr_clean <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character) %>%
                dplyr::mutate_all(trimws, "both")
}

