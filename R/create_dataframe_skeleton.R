#' Takes a character vector and creates a 0-row dataframe with the vectors as column names
#' @export

create_dataframe_skeleton <-
        function(column_names) {
                x <- data.frame(matrix(ncol = length(column_names)), stringsAsFactors = FALSE)
                colnames(x) <- column_names
                x <- x[-1,]
                return(x)
        }
