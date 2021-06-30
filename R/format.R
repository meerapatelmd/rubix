#' @title
#' Format Column Names
#' @description
#' Format is defined as
#' 1) all letters converted to upper,
#' 2) trailing or leading whitespace removed,
#' 3) all punctuation or spaces of any length replaced with a single underscore,
#' 4) any trailing underscores are removed, which represent terminal punctuation in the column name
#' @seealso
#'  \code{\link[dplyr]{select_all}}
#' @rdname format_colnames
#' @export
#' @importFrom dplyr rename_all %>%

format_colnames <-
  function(data,
           order = c(
             "btrim",
             "tolower",
             "uscore",
             "rm_trailing_punct",
             "rm_leading_punct"
           )) {
    expr <- paste(sprintf("dplyr::rename_all(%s)", order), collapse = " %>% ")
    expr <- paste0("data %>% ", expr)

    eval(rlang::parse_expr(expr))
  }





#' @title
#' Standardize List Names
#' @description
#' This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[centipede]{trimws}}
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#' @rdname format_names
#' @export
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom stringr str_replace_all str_remove_all

format_names <-
  function(list,
           order = c(
             "btrim",
             "tolower",
             "uscore",
             "rm_trailing_punct",
             "rm_leading_punct"
           )) {
    expr <- paste(sprintf("purrr::map(%s)", order), collapse = " %>% ")
    expr <- paste0("names(list) %>% ", expr)

    names(list) <- eval(rlang::parse_expr(expr))

    list
  }



#' @export

uscore <-
  function(vector) {
    stringr::str_replace_all(vector, "[^0-9a-zA-Z]", "_")
  }

#' @export

rm_leading_punct <-
  function(vector) {
    stringr::str_remove_all(vector, "^[[:punct:]]{1,}")
  }


#' @export

rm_trailing_punct <-
  function(vector) {
    stringr::str_remove_all(vector, "[[:punct:]]{1,}$")
  }


#' @export

btrim <-
  function(vector) {
    trimws(
      x = vector,
      which = "both"
    )
  }
