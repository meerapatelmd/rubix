#' Add timestamp to the entire dataframe at position 1 with the provided column name
#' @param new_col_name defaults to "TIMESTAMP"
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'

add_timestamp_column <-
  function(dataframe, new_col_name = "TIMESTAMP") {
      new_col_name <- enquo(new_col_name)

      dataframe %>%
              mutate(!!new_col_name := as.character(Sys.time())) %>%
              select(!!new_col_name, everything())

  }
