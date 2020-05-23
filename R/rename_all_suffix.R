#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_all_suffix <-
        function(dataframe,
                 suffix) {
                dataframe %>%
                        dplyr::rename_all(function(x) paste0(x, suffix))
        }







