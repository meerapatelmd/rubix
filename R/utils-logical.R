#' @export

col_exists <-
  function(data, col) {
    x <-
      tryCatch(
        expr =
          data %>%
            dplyr::select({{ col }}),
        error = function(e) NULL
      )

    if (is.null(x)) {
      FALSE
    } else {
      TRUE
    }
  }
