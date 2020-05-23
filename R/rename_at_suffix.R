#' Rename all with suffix
#' @importFrom dplyr rename_all
#' @export

rename_at_suffix <-
        function(dataframe,
                 ...,
                 suffix) {
                
                cols <- enquos(...)
                
                dataframe %>%
                        dplyr::rename_at(vars(!!!cols), function(x) paste0(x, suffix))
        }







