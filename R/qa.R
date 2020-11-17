#' @title 
#' Names of Duplicate Columns
#' 
#' @description 
#' Return the column names in a dataframe that contain duplicate values of another column. 
#' 
#' @rdname qa_duplicate_cols
#' @example inst/examples/qa.R
#' 
#' @importFrom purrr set_names keep
#' @export


duplicated_cols <-
        function(data) {
                
                cols_to_list(data = data) %>%
                        duplicated() %>%
                        purrr::set_names(names(data)) %>%
                        purrr::keep(~ . == TRUE) %>%
                        names()
                
                
        }

