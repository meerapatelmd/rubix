#' General Dataframe Cleanup
#' Convert all columns to character class and trim all left and right white spaces.
#' @param dataframe input dataframe
#' @importFrom dplyr mutate_all
#' @export

mutate_all_as_char <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character)
}

