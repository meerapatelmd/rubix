#' Get the number of rows that differ between 2 dataframes
#' @importFrom dplyr setdiff
#' @export

setdiff_nrow <-
        function(x, y) {
                
                nrow(dplyr::setdiff(x, y))
                
        }


setdiff_col_match <- 
        function(data_a,
                 data_b) {
                
                
                cols <- colnames(data_a)[colnames(data_a) %in% colnames(data_b)]
                

                        list(A = data_a,
                             B = data_b) %>%
                        purrr::map(function(x) 
                                                x %>% 
                                                dplyr::select(dplyr::all_of(cols))) %>%
                        purrr::reduce(setdiff)
                
                
                
        }




compare_cols <- 
        function(
          data_a,
          data_b) {
                
                
                

                a <- tibble::tibble(column_a = colnames(data_a)) %>%
                        tibble::rowid_to_column(var = "position_a") %>%
                        dplyr::mutate(column = column_a)
                
                b <- tibble::tibble(column_b= colnames(data_b)) %>%
                        tibble::rowid_to_column(var = "position_b") %>%
                        dplyr::mutate(column = column_b)
                
                dplyr::full_join(a, 
                                 b, 
                                 by = "column")

                
        }
