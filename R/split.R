#' @title
#' Split a Dataframe
#'
#' @inheritParams wrapper_args
#' @inheritParams base::split
#' @seealso
#'  \code{\link[base]{split}}
#'  \code{\link[dplyr]{select}}
#' @rdname split_by
#' @example inst/examples/split.R
#' @export
#' @importFrom dplyr select


split_by <-
  function(data,
           col,
           drop = FALSE,
           sep = ".",
           lex.order = FALSE) {
    base::split(data,
      factor(data %>%
        dplyr::select({{ col }}) %>%
        unlist() %>%
        unname()),
      drop = drop,
      sep = sep,
      lex.order = lex.order
    )
  }






#' @title
#' Split a Dataframe
#'
#' @description
#' Split and deselect the column used to split by in a single function call.
#'
#' @inheritParams wrapper_args
#' @inheritParams base::split
#' @seealso
#'  \code{\link[base]{split}}
#'  \code{\link[dplyr]{select}}
#' @rdname split_deselect
#' @example inst/examples/split.R
#' @export
#' @importFrom dplyr select


split_deselect <-
  function(data,
           col,
           drop = FALSE,
           sep = ".",
           lex.order = FALSE) {
    output <-
      split_by(
        data = data,
        col = {{ col }},
        drop = drop,
        sep = sep,
        lex.order = lex.order
      )

    output %>%
      map_names_set(function(y) {
        y %>%
          dplyr::select(-{{ col }})
      })
  }
