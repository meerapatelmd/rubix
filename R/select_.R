#' @title
#' Deselect columns that are all NA
#' @seealso
#'  \code{\link[dplyr]{select_all}}
#' @rdname deselect_if_all_na
#' @export
#' @importFrom dplyr select_if %>%
#' @example inst/examples/deselect.R

deselect_if_all_na <-
  function(data) {
    na_cols <-
      data %>%
      dplyr::select_if(all_is_na) %>%
      colnames()

    data %>%
      dplyr::select_at(dplyr::vars(!dplyr::all_of(na_cols)))
  }





#' @title
#' Deselect columns that are all NA
#' @seealso
#'  \code{\link[dplyr]{select_all}}
#' @rdname deselect_if_all_same
#' @export
#' @importFrom dplyr select_if %>%
#' @example inst/examples/deselect.R


deselect_if_all_same <-
  function(data,
           na.rm = FALSE) {
    same_cols <-
      data %>%
      dplyr::select_if(~ all_same_value(., na.rm = na.rm)) %>%
      colnames()

    data %>%
      dplyr::select_at(dplyr::vars(!dplyr::all_of(same_cols)))
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
