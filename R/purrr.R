#' Map and set_names function from purrr package in tandem
#' @description This function automatically sets the names in the list or atomic vector from the input data without having to make 2 separate function calls to purrr::map and purrr::set_names.
#' @param x input
#' @param .f function in purrr::map
#' @param ... additional parameters for the purrr::map function
#' @importFrom purrr set_names
#' @importFrom purrr map
#' @export

map_names_set <-
  function(x, .f, ...) {
    if (is.list(x)) {
      x %>%
        purrr::set_names(names(x)) %>%
        purrr::map(.f, ...)
    } else {
      x %>%
        purrr::set_names(as.character(x)) %>%
        purrr::map(.f, ...)
    }
  }
