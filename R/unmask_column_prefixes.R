#' For R objects where column names are prefixed for object names, restores the full column names
#' @importFrom stringr str_remove
#' @export

unmask_column_prefixes <-
        function(dataframe) {
                dataframe_name <- stringr::str_remove(deparse(substitute(dataframe)), "[$]{1}.*$")
                dataframe_prefix <- paste0("^", dataframe_name, "_")
                current_column_names <- colnames(dataframe)
                
                new_column_names <- current_column_names
                for (i in 1:length(current_column_names)) {
                        current_column_name <- current_column_names[i]
                        if (grepl(dataframe_prefix, current_column_name) == FALSE) {
                                new_column_names[i] <- paste(dataframe_name, current_column_name, sep = "_")
                        }
                }
                colnames(dataframe) <- new_column_names
                return(dataframe)
        }
