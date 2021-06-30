#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param .x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[dplyr]{select_all}},\code{\link[dplyr]{funs}}
#' @rdname rename_as
#' @export

rename_as <-
  function(data, new_colnames) {
    if (ncol(data) != length(new_colnames)) {
      stop("vector not the same length as columns")
    }

    colnames(data) <- new_colnames
    data
  }



#' @title
#' Rename all columns with a prefix
#' @seealso
#'  \code{\link[dplyr]{select_all}}
#' @rdname rename_all_prefix
#' @export
#' @importFrom dplyr rename_all %>%


rename_all_prefix <-
  function(data,
           prefix) {
    data %>%
      dplyr::rename_all(function(x) paste0(prefix, x))
  }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_all
#' @importFrom stringr str_remove_all
#' @export

rename_all_remove <-
  function(data,
           pattern) {
    data %>%
      dplyr::rename_all(function(x) {
        stringr::str_remove_all(x,
          pattern = pattern
        )
      })
  }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_all
#' @importFrom stringr str_replace_all
#' @export

rename_all_replace <-
  function(data,
           pattern,
           replacement) {
    data %>%
      dplyr::rename_all(function(x) {
        stringr::str_replace_all(x,
          pattern = pattern,
          replacement = replacement
        )
      })
  }





#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_all_suffix <-
  function(data,
           suffix) {
    data %>%
      dplyr::rename_all(function(x) paste0(x, suffix))
  }












#' Rename all columns with a prefix
#' @importFrom dplyr rename_all
#' @export


rename_all_with_prefix <-
  function(data,
           prefix) {
    data %>%
      dplyr::rename_all(function(x) paste0(prefix, x))
  }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_all
#' @importFrom stringr str_replace_all
#' @export

rename_all_with_replace <-
  function(data,
           pattern,
           replacement) {
    data %>%
      dplyr::rename_all(function(x) {
        stringr::str_replace_all(x,
          pattern = pattern,
          replacement = replacement
        )
      })
  }





#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_all_with_suffix <-
  function(data,
           suffix) {
    data %>%
      dplyr::rename_all(function(x) paste0(x, suffix))
  }

















#' Rename all columns with a prefix
#' @importFrom dplyr rename_at
#' @export


rename_at_prefix <-
  function(data,
           ...,
           prefix) {
    cols <- dplyr::enquos(...)

    data %>%
      dplyr::rename_at(vars(!!!cols), function(x) paste0(prefix, x))
  }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_at
#' @importFrom stringr str_remove_all
#' @export

rename_at_remove <-
  function(data,
           ...,
           pattern) {
    cols <- dplyr::enquos(cols)

    data %>%
      dplyr::rename_at(vars(!!!cols), function(x) {
        stringr::str_remove_all(x,
          pattern = pattern
        )
      })
  }





#' Replace a pattern in all column names
#' @importFrom dplyr rename_at
#' @importFrom stringr str_remove_all
#' @export

rename_at_replace <-
  function(data,
           ...,
           pattern,
           replacement) {
    cols <- dplyr::enquos(cols)

    data %>%
      dplyr::rename_at(vars(!!!cols), function(x) {
        stringr::str_replace_all(x,
          pattern = pattern,
          replacement = replacement
        )
      })
  }





#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_at_suffix <-
  function(data,
           ...,
           suffix) {
    cols <- enquos(...)

    data %>%
      dplyr::rename_at(vars(!!!cols), function(x) paste0(x, suffix))
  }
