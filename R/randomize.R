#' @title
#' Randomize a Dataframe
#' @example inst/examples/randomize.R
#' @param data A dataframe or tibble.
#' @rdname randomize
#' @export

randomize <-
  function(data) {
    data[
      sample(1:nrow(data),
        size = nrow(data),
        replace = FALSE
      ),
    ]
  }
