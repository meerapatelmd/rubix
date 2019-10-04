#' Removes lengthy column prefixes that match the R object name for better visualization
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @export

mask_column_prefixes <-
        function(dataframe) {
                dataframe_name <- stringr::str_remove(deparse(substitute(dataframe)), "[$]{1}.*$")
                dataframe_prefix <- paste0("^", dataframe_name, "_")
                
                args <- colnames(dataframe)
                args <- stringr::str_remove_all(args, pattern = dataframe_prefix)
                
                colnames(dataframe) <- args
                return(dataframe)
        }