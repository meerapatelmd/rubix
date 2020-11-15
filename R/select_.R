#' @title
#' Deselect columns that are all NA
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname deselect_if_all_na
#' @export 
#' @importFrom dplyr select_if %>%


deselect_if_all_na <-
        function(.data) {
                
                all_is_na <- 
                        function(vector) {
                                return(all(is.na(vector)))
                        }
                
                all_is_not_na <-
                        function(vector) {
                                return(!(all_is_na(vector)))
                        }
                
                .data %>%
                        dplyr::select_if(all_is_not_na)
        }





#' @title
#' Deselect columns that are all NA
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname deselect_if_all_same
#' @export 
#' @importFrom dplyr select_if %>%


deselect_if_all_same <-
        function(dataframe) {
                
                all_is_same <- 
                        function(vector) {
                                length(unique(vector)) == 1
                        }
                
                all_is_not_same <-
                        function(vector) {
                                !all_is_same(vector)
                        }
                
                dataframe %>%
                        dplyr::select_if(all_is_not_same)
        }





#' @title
#' Selects columns and get the distinct values back
#' @seealso 
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}}
#' @rdname select_distinct
#' @export 
#' @importFrom dplyr select distinct %>%

select_distinct <-
        function(data, ...) {
                
                vars <- enquos(...)

                data %>%
                        dplyr::select(!!!vars) %>%
                        dplyr::distinct()
        }





#' @title
#' Select columns that are all NA
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname select_if_all_na
#' @export 
#' @importFrom dplyr select_if %>%


select_if_all_na <-
        function(dataframe) {
                
                all_is_na <- 
                        function(vector) {
                                return(all(is.na(vector)))
                        }

                
                dataframe %>%
                        dplyr::select_if(all_is_na)
        }





