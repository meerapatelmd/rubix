#' Release all the columns in a dataframe into vectors of the same name
#' @importFrom dplyr select
#' @importFrom purrr map2
#' @export

release_df <-
        function(dataframe) {
                column_names <- colnames(dataframe)
                
                output1 <-
                column_names %>%
                        map_names_set(function(x) dataframe %>%
                                                        dplyr::select(x) %>%
                                                        unlist() %>%
                                                        unname())

                invisible(
                output1 %>%
                        purrr::map2(names(output1),
                                    function(xx,yy) assign(yy,
                                                         value = xx,
                                                         envir = globalenv())))
        }
