#' Filter for the first row
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @export

filter_first_row <-
        function(.data, invert = FALSE) {
                if (invert) {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() != 1)
                        

                } else {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() == 1)
                        
                        
                }

        }





#' @title
#' Load Summary Function Library     
#' @seealso 
#'  \code{\link[centipede]{no_na}}
#' @rdname loadSummaryFnLibrary
#' @export 
#' @importFrom centipede no_na
#' @importFrom magrittr %>%

loadSummaryFnLibrary <- 
        function() {
                summaryFunLibrary <<- 
                        list(numerical = list(
                                MEAN = ~mean(., na.rm = TRUE), 
                                MEAN_NA = ~mean(., na.rm = FALSE), 
                                MEDIAN = ~median(., na.rm = TRUE), 
                                MEDIAN_NA = ~median(., na.rm = FALSE), 
                                SD = ~sd(., na.rm = TRUE), 
                                SD_NA = ~sd(., na.rm = FALSE), 
                                MAX = ~max(., na.rm = TRUE), 
                                MAX_NA = ~max(., na.rm = FALSE), 
                                MIN = function(x) min(x, na.rm = TRUE), 
                                MIN_NA = function(x) min(x, na.rm = FALSE),
                                SUM = ~sum(.,na.rm = TRUE),
                                SUM_NA = ~sum(.,na.rm = FALSE),
                                DISTINCT_LENGTH = ~length(unique(.)),
                                NA_LENGTH = ~length(.[is.na(.)]), 
                                BLANK_LENGTH = ~length(.[. %in%  c("")]), 
                                DISTINCT_STR = ~paste(sort(unique(as.character(.))), collapse = "|"),
                                DISTINCT_STR_NA = ~paste(sort(unique(as.character(.)) %>% centipede::no_na()), collapse = "|")
                        ),
                        categorical = list(
                                COUNT = ~length(.), 
                                DISTINCT_COUNT = ~length(unique(.)), 
                                NA_COUNT = ~length(.[is.na(.)]), 
                                NA_STR_COUNT = ~length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]), 
                                BLANK_COUNT = ~length(.[. %in%  c("")]), 
                                DISTINCT_VALUES = ~paste(sort(unique(as.character(.))), collapse = "|"),
                                DISTINCT_VALUES_NA = ~paste(sort(unique(as.character(.)) %>% centipede::no_na()), collapse = "|")
                        )
                        )
        }





#' Release all the columns in a dataframe into vectors of the same name
#' @importFrom dplyr select
#' @importFrom purrr map2
#' @export

release_df <-
        function(dataframe) {
                
                .Deprecated()
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





#' Slice first row
#' @importFrom dplyr slice
#' @export

slice_first_row <-
        function(dataframe) {
                dataframe %>%
                        dplyr::slice(-1)
        }







