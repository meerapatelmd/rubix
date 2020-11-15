#' General Dataframe Cleanup
#' Convert all columns to character class and trim all left and right white spaces.
#' @param dataframe input dataframe
#' @importFrom dplyr mutate_all
#' @export

mutate_all_as_char <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character)
}






#' Replace "" with <NA>
#' @return A tibble
#' @importFrom tibble as_tibble
#' @export

mutate_all_blank_to_na <- 
        function(.data) {
                .data <-
                        .data %>%
                        tibble::as_tibble()
                
                .data[.data == ""] <- NA_character_
                return(.data)
        }





#' Mutate all "NA" to <NA>
#' @return A tibble
#' @importFrom tibble as_tibble
#' @export

mutate_all_na_str_to_na <- 
        function(.data) {
                .data <-
                        .data %>%
                        tibble::as_tibble()
                
                .data[.data == "NA"] <- NA_character_
                return(.data)
        }





#' Substitute all true NA values in a dataframe as blank
#' @importFrom dplyr mutate_all
#' @export
mutate_all_na_to_blank <-
        function(dataframe, include_na_as_string = TRUE) {
                x <- dataframe
                x[is.na(x)] <- ""

                if (include_na_as_string == TRUE) {
                        x <- x %>%
                                dplyr::mutate_all(str_replace_all, "NA", "")
                } else {
                        x <- x
                }
                return(x)
        }










#' Mutate cols to integer data type
#' @import dplyr
#' @export

mutate_to_integer <- 
        function(.data, ...) {
                
                cols <- enquos(...)
                
                .data %>%
                        dplyr::mutate_at(vars(!!!cols), as.integer)
                
                
        }





