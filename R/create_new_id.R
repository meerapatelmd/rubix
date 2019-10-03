#' Creates the next primary key 
#' @param id_variable column that contains the primary key
#' @importFrom dplyr select
#' @importFrom dplyr mutate_all
#' @export
create_new_pk <-
        function(dataframe, id_variable) {
                id_variable <- enquo(id_variable)
                
                as.character(
                1 + (dataframe %>%
                             dplyr::select(!!id_variable) %>%
                             dplyr::mutate_all(as.integer) %>%
                             unlist() %>%
                             max()
                )
                )
        }
