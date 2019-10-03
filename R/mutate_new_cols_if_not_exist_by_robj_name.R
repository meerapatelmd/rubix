#' Adds a column if it doesn't exist in the dataframe for each object in the global environment
#' @param robj_pattern regular expression pattern of robj names to be added to the dataframe
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export

mutate_new_cols_if_not_exist_by_robj_name <-
        function(dataframe, robj_pattern) {
                df_name <- deparse(substitute(dataframe))

                new_columns <- objects(pattern = robj_pattern,
                                       envir = globalenv())

                if (length(new_columns) > 0) {
                        for (i in 1:length(new_columns)) {
                                column_name <- new_columns[i]

                                if (!(column_name %in% colnames(dataframe))) {

                                        column_name <- enquo(column_name)

                                        x <-
                                                dataframe %>%
                                                dplyr::mutate(!!column_name := "")

                                        assign(df_name, x, envir = globalenv())
                                }
                        }
                }

        }

