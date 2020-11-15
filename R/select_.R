#' @title
#' Deselect columns that are all NA
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname deselect_if_all_na
#' @export 
#' @importFrom dplyr select_if %>%


deselect_if_all_na <-
        function(data) {
                
                data %>%
                        dplyr::select_if(all_not_na)
        }





#' @title
#' Deselect columns that are all NA
#' @seealso 
#'  \code{\link[dplyr]{select_all}}
#' @rdname deselect_if_all_same
#' @export 
#' @importFrom dplyr select_if %>%


deselect_if_all_same <-
        function(data,
                 na.rm = FALSE) {

                
                data %>%
                        dplyr::select_if(~ all_same_value(., na.rm = na.rm))
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
        function(data) {
                
                data %>%
                        dplyr::select_if(all_is_na)
        }





