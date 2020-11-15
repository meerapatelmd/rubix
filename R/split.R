#' Split a dataframe into a list with the column removed
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export


split_by <-
        function(data, 
                 column,
                 drop = FALSE,
                 sep = ".",
                 lex.order = FALSE) {

                split(data, 
                      factor(data %>% 
                                     dplyr::select({{ column }}) %>%
                                     unlist() %>%
                                     unname()),
                      drop = drop,
                      sep = sep,
                      lex.order = lex.order)
        }






#' Split a dataframe into a list with the column removed
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @export


split_deselect <-
        function(data, 
                 column,
                 drop = FALSE,
                 sep = ".",
                 lex.order = FALSE) {

                output <- 
                        split_by(data = data,
                                 column = {{ column }})
                
                output %>%
                        map_names_set(function(y) y  %>%
                                                        dplyr::select(-{{ column }}))
                
                
        }






