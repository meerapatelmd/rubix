#' Remove all multibyte characters in the dataframe
#' @importFrom dplyr mutate_all
#' @importFrom stringr str_replace_all
#' @export

rm_multibyte_chars <-
    function(dataframe) {
        return(
            dataframe %>%
           dplyr::mutate_all(stringr::str_replace_all, "[^ -~]", ""))
        
    }