#' Parse a Date Var
#' @description This function takes a character variable of dates in various formats and parses it using a battery of parsing functions.
#' @return list of dataframes for each parsed date variable along with the parsed result resulting from a coalesce of all date parsing options
#' @param origin_eval if TRUE, includes evaluation of the variables as dates with origins of 1900-01-01 and 1970-01-01. Since the evaluation of origin is temperamental (ie 91884 is parsed into a date in 1997 which would be difficult to weed out downstream since it is a realistic date), it is not coalesced with the other parsed columns and is reported separately in the output. 
#' @importFrom dplyr enquos
#' @importFrom rlang as_name
#' @import dplyr
#' @importFrom lubridate ymd
#' @importFrom lubridate mdy
#' @importFrom lubridate ydm
#' @importFrom lubridate myd
#' @importFrom lubridate dmy 
#' @importFrom lubridate dym
#' @export

parse_date_var <-
        function(.data, ..., quiet = TRUE, origin_eval = FALSE) {
                # test_vector <- c("91884", "", NA, "091884", "09984", "090984", "09/18/1984", "1984-09-18", "9/18/84", "9/9/84", "9-9-84", "9-18-84", "12-18-84")
                # test_df <- tibble(test_v = test_vector)
                
                cols <- enquos(...)
                
                col_labels <- sapply(cols, rlang::as_name)
                
                output <-
                        cols %>%
                        purrr::map(function(x) .data %>%
                                           dplyr::select(!!x) %>%
                                           dplyr::mutate(ymd = lubridate::ymd(!!x, quiet = quiet),
                                                         mdy = lubridate::mdy(!!x, quiet = quiet),
                                                         ydm = lubridate::ydm(!!x, quiet = quiet),
                                                         myd = lubridate::myd(!!x, quiet = quiet),
                                                         dmy = lubridate::dmy(!!x, quiet = quiet),
                                                         dym = lubridate::dym(!!x, quiet = quiet))) %>%
                        purrr::set_names(col_labels)
                
                
                if (origin_eval) {
                        
                        output <-
                                output %>%
                                purrr::map2(cols, function(x,y) x %>%
                                                    dplyr::mutate(origin_19700101 = as.Date(suppressWarnings(as.double(!!y)), origin = "1970-01-01")) %>%
                                                    dplyr::mutate(origin_19000101 = as.Date(suppressWarnings(as.double(!!y)), origin = "1900-01-01")))
                        
                        
                }
                
                output <- 
                        output %>% 
                        purrr::map2(cols, function(x,y) x %>%
                                            dplyr::mutate(!!y := coalesce(ymd, mdy, ydm, myd, dmy, dym)) %>%
                                                dplyr::select(!!y, any_of(starts_with("origin"))) %>%
                                                rename_all_prefix("parsed_")) 
                
                output <-
                        output %>%
                        purrr::map2(cols, function(x,y) .data %>%
                                                                dplyr::select(!!y, any_of(starts_with("origin"))) %>%
                                                                dplyr::bind_cols(x))
                return(output)
                
        }
