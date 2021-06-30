#' Parse a Date Column
#' @details
#' Includes evaluation of the variables as dates with origins of 1900-01-01 and 1970-01-01. Since the evaluation of origin is temperamental (ie 91884 is parsed into a date in 1997 which would be difficult to weed out downstream since it is a realistic date), it is not coalesced with the other parsed columns and is reported separately in the output.
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[lubridate]{ymd}}
#' @rdname parse_date_col
#' @export
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd mdy ydm myd dmy dym
#' @example inst/examples/parse_date_col.R

parse_date_col <-
  function(data,
           col,
           quiet = TRUE,
           tz = NULL,
           locale = Sys.getlocale("LC_TIME")) {
    data %>%
      dplyr::mutate(
        ymd = lubridate::ymd({{ col }}, quiet = quiet, tz = tz, locale = locale),
        mdy = lubridate::mdy({{ col }}, quiet = quiet, tz = tz, locale = locale),
        ydm = lubridate::ydm({{ col }}, quiet = quiet, tz = tz, locale = locale),
        myd = lubridate::myd({{ col }}, quiet = quiet, tz = tz, locale = locale),
        dmy = lubridate::dmy({{ col }}, quiet = quiet, tz = tz, locale = locale),
        dym = lubridate::dym({{ col }}, quiet = quiet, tz = tz, locale = locale)
      ) %>%
      dplyr::mutate(origin_19700101 = as.Date(suppressWarnings(as.double({{ col }})), origin = "1970-01-01")) %>%
      dplyr::mutate(origin_19000101 = as.Date(suppressWarnings(as.double({{ col }})), origin = "1900-01-01"))
  }
