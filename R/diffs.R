#' @title 
#' Diff Functions
#' 
#' @param x A dataframe or tibble.
#' @param y A dataframe or tibble.
#' 
#' @name diffs
NULL


#' @title 
#' Number of Different Rows between 2 Dataframes
#' 
#' @inheritParams diffs
#' @rdname setdiff_nrow
#' @family diff functions
#' @export
#' @example inst/examples/diffs.R

setdiff_nrow <-
        function(x, y) {
                
                nrow(setdiff(x, y))
                
        }

#' @title 
#' Set Difference between Overlapping Columns
#' @description 
#' Preemptively select for columns that overlap between 2 dataframes before calling the `setdiff()` function.
#' @inheritParams diffs
#' @seealso 
#'  \code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname setdiff_col_match
#' @family diff functions
#' @export 
#' @importFrom purrr map reduce
#' @importFrom dplyr select all_of
#' @example inst/examples/diffs.R

setdiff_col_match <- 
        function(x,
                 y) {
                
                
                cols <- colnames(x)[colnames(x) %in% colnames(y)]
                

                list(X = x,
                     Y = y) %>%
                        purrr::map(function(x) 
                                                x %>% 
                                                dplyr::select(dplyr::all_of(cols))) %>%
                        purrr::reduce(setdiff)
                
                
        }


#' @title 
#' Compare Column Names and Positions
#' @description 
#' Compare column names and positions for 2 dataframes. 
#' @inheritParams diffs
#' @seealso 
#'  \code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname setdiff_col_match
#' @family diff functions
#' @export 
#' @importFrom purrr map reduce
#' @importFrom dplyr select all_of
#' @example inst/examples/diffs.R

compare_cols <- 
        function(
          x,
          y) {
                
                
                

                a <- tibble::tibble(column_x = colnames(x)) %>%
                        tibble::rowid_to_column(var = "position_x") %>%
                        dplyr::mutate(x_and_y_columns = column_x)
                
                b <- tibble::tibble(column_y = colnames(y)) %>%
                        tibble::rowid_to_column(var = "position_y") %>%
                        dplyr::mutate(x_and_y_columns = column_y)
                
                dplyr::full_join(a, 
                                 b, 
                                 by = "x_and_y_columns")

                
        }
