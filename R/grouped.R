#' @title
#' Aggregate Function
#' @seealso
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}}
#' @rdname grouped_paste
#' @export
#' @importFrom dplyr enquos enquo group_by summarize_at ungroup %>%

grouped_paste <-
  function(data,
           ...,
           unique = TRUE,
           paste_col,
           collapse = "|") {
    cols <- dplyr::enquos(...)


    if (unique) {
      data %>%
        dplyr::group_by_at(dplyr::vars(!!!cols)) %>%
        dplyr::summarize_at(vars({{ paste_col }}), function(x) paste(unique(x), collapse = collapse)) %>%
        dplyr::ungroup()
    } else {
      data %>%
        dplyr::group_by_at(dplyr::vars(!!!cols)) %>%
        dplyr::summarize_at(vars({{ paste_col }}), function(x) paste(x, collapse = collapse)) %>%
        dplyr::ungroup()
    }
  }
