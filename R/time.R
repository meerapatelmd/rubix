#' @title
#' Add a Datetime Column to Position 1
#'
#' @param data A dataframe or tibble.
#' @param var Character string of new column name.

#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname datetime_to_col
#' @example inst/examples/time.R
#' @export
#' @importFrom dplyr mutate select everything

datetime_to_col <-
  function(data, var = "datetime") {
    data %>%
      dplyr::mutate({{ var }} := Sys.time()) %>%
      dplyr::select(
        {{ var }},
        dplyr::everything()
      )
  }


#' @title
#' Add a Date Column to Position 1
#'
#' @param data A dataframe or tibble.
#' @param var Character string of new column name.

#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname date_to_col
#' @example inst/examples/time.R
#' @export
#' @importFrom dplyr mutate select everything


date_to_col <-
  function(data, var = "date") {
    data %>%
      dplyr::mutate({{ var }} := Sys.Date()) %>%
      dplyr::select(
        {{ var }},
        dplyr::everything()
      )
  }
