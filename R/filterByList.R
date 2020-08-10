#' Filter By a List and Other Filters
#' @description A List object with name as filter fields and values can be used by splicing it with !!! when included as an argument.
#' @rdname filterByList
#' @export 
#' @importFrom rlang list2 parse_expr
#' @importFrom purrr map_if map2
#' @importFrom cave vector_to_string
#' @importFrom secretary typewrite_bold typewrite


filterByList <- 
        function(.data, ...) {
                
                Settings <- rlang::list2(...)
                
                settings <-
                        Settings %>%
                        purrr::map_if(function(x) !(length(x) == 1 && is.na(x)),
                                      function(x) cave::vector_to_string(x)) %>%
                        purrr::map_if(function(x) (length(x) == 1 && is.na(x)),
                                      function(x) paste0("c(", x, ")")) %>%
                        purrr::map2(names(Settings),
                                    function(x,y) paste("\t", y,"%in%", x, collapse = " ")) %>%
                        unlist() %>%
                        unname() %>%
                        paste(collapse = ",\n")
                
                secretary::typewrite_bold("Settings:")
                secretary::typewrite(settings)
                
                cat("\n")
                
                eval(rlang::parse_expr(paste0(".data %>% dplyr::filter(", settings, ")")))
                
        }
