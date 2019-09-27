# Mutates all columns of a dataframe to character and trims all white spaces
#       @param  dataframe
#       
#       @return cleaned up dataframe
call_mr_clean <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character) %>%
                dplyr::mutate_all(trimws, "both")
}

