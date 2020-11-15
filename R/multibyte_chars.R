#' @export

rm_multibyte <- 
        function(vector) {
                
                stringr::str_remove_all(string = vector,
                                        pattern = "[^ -~]")
        }



#' Remove all multibyte characters in the dataframe
#' @importFrom dplyr mutate_all
#' @importFrom stringr str_replace_all
#' @export

rm_multibyte_chars <-
    function(data) {
            
            cols <- char_cols(data = data)

            data %>%
                    dplyr::mutate_at(
                            dplyr::vars(dplyr::all_of(cols)),
                                        ~ rm_multibyte(.))
            
        
    }