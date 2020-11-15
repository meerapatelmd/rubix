#' @title 
#' Arrange by a Given Column as Integer
#' @description 
#' Perform an arrange function call on a dataframe with the values of the target column as an integer class.
#' @param .data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION
#' @param desc PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{arrange}}
#' @rdname arrange_as_integer
#' @export 
#' @importFrom dplyr enquo arrange %>%


arrange_as_integer <-
        function(.data, column, desc = FALSE) {
                
                column <- dplyr::enquo(column)
                

                if (desc == FALSE) {
                        .data %>%
                                dplyr::arrange(as.integer(!!column))
                } else {
                        .data %>%
                                dplyr::arrange(desc(as.integer(!!column)))
                }

        }





#' Standardize Column Names
#' This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
#' @inheritParams call_mr_clean
#' @importFrom  dplyr rename_all
#' @export

cleanup_colnames <-
        function(dataframe) {
                .Deprecated()
                x <-
                        dataframe %>%
                                dplyr::rename_all(trimws, "both") %>%
                                dplyr::rename_all(toupper) %>%
                                dplyr::rename_all(str_replace_all, "[[:punct:]]{1,}", "_") %>%
                                dplyr::rename_all(str_remove_all, "[_]{1}$") %>%
                                dplyr::rename_all(str_replace_all, " {1,}", "_")
                colnames(x) <- gsub("[_]{2,}", "_", colnames(x))

                return(x)
        }





#' Standardize List Names
#' This function takes a dataframe and changes column names in the following order: 1) Trims whitespace on both sides, 2) converts to uppercase, 3) replaces punctuation of any length with a single underscore, 4) removes trailing underscores from native column names that often times have trailing punctuation (usually decimals)
#' @importFrom purrr map
#' @importFrom centipede trimws
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @export

cleanup_listnames <-
        function(list) {
                .Deprecated()
                names(list) <-
                        names(list) %>%
                                purrr::map(centipede::trimws, "both") %>%
                                purrr::map(toupper) %>%
                                purrr::map(stringr::str_replace_all, "[[:punct:]]{1,}", "_") %>%
                                purrr::map(stringr::str_remove_all, "[_]{1}$") %>%
                                purrr::map(stringr::str_replace_all, " {1,}", "_")
                names(list) <- gsub("[_]{2,}", "_", names(list))

                return(list)
        }





#' @title
#' Coalesce 1+ Fields to a New Field
#' @description 
#' (Deprecated) This is 1 of 2 coalesce functions. This one takes fields that are not the same as the proposed new column name. For example, a dataframe with fields "A", "B", and "C" can be coalesced into a field "D". If all the values need to be coalesced into an existing field such as "A", "B", and "C" to "A", use coalesce_at function instead.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{coalesce}}
#' @rdname coalesce_into
#' @export 
#' @importFrom dplyr enquos enquo select mutate coalesce %>%


coalesce_into <- 
        function(.data,
                 into,
                 ...,
                 remove = FALSE) {
                
                .Deprecated("coalesce_to")
                
                cols <- dplyr::enquos(...)
                into <- dplyr::enquo(into)
                
                if (remove) {
                        
                        remove_cols <- 
                                .data %>%
                                dplyr::select(!!!cols) %>%
                                colnames()
                        
                        .data %>% 
                                dplyr::mutate(!!into := dplyr::coalesce(!!!cols)) %>%
                                dplyr::select(-all_of(remove_cols))
                        
                } else {
                        
                        .data %>% 
                                dplyr::mutate(!!into := dplyr::coalesce(!!!cols))
                        
                }
                        
        }





#'Add the current date
#'@param punct default is FALSE. If true, returns date in YYYYmmdd format. Otherwise YYYY-mm-dd format.
#'@export

dated <- function(punct = FALSE) {
        if (punct == FALSE) {
                return(as.character(Sys.Date()))
        } else {
                return(as.character(format(Sys.Date(), "%Y%m%d")))
        }
}





#' Loop a filter over words in a string until there are zero rows
#' This function splits a string based on the provide split argument, and recursively filters a dataframe for those words at a given column. It differs from the filter_by_words_in_str function because it returns a dataframe if the loop ends up evaluating to zero rows.
#' @inheritParams filter_by_words_in_str
#' @importFrom secretary typewrite
#' @export

filter_by_words_in_str_before_zero <-
        function(string, split, dataframe, col) {
                col <- dplyr::enquo(col)
                Args <- strsplit(string, split = split) %>% unlist()
                
                while (length(Args) > 0) {
                                dataframe2 <-
                                        dataframe %>%
                                        filter_at_grepl(col = !!col, Args[1])
                                
                                
                                if (nrow(dataframe2) == 0) {
                                        secretary::typewrite("Dataframe evaluates to zero rows when filtering for ", Args[1], ". Returning dataframe immediately prior to this filter.")
                                        return(dataframe)
                                } else {
                                        dataframe <- dataframe2
                                        rm(dataframe2)
                                        Args <- Args[-1]
                                }
                }
                secretary::typewrite("All words in the string were filtered for.")
                return(dataframe)
        }





#' Loop a filter over words in a string
#' This function splits a string based on the provide split argument, and recursively filters a dataframe for those words at a given column.
#' @param string string of words or other tokens to loop the filter over
#' @param split split argument for the strsplit function applied to the string parameter
#' @param dataframe input data
#' @param col column to filter against
#' @importFrom secretary typewrite
#' @importFrom dplyr enquo
#' @export

filter_by_words_in_str <-
        function(string, split, dataframe, col) {
                
                .Deprecated()
                
                col <- dplyr::enquo(col)
                
                Args <- strsplit(string, split = split) %>% unlist()
                
                while (length(Args) > 0) {
                                dataframe <-
                                        dataframe %>%
                                        filter_at_grepl(col = !!col, Args[1])
                                
                                Args <- Args[-1]
                }
                return(dataframe)
        }





#' Filters a dataframe for a date in YYYY-mm-dd format in a timestamp column
#' @param .data .data with timestamp column
#' @param timestamp_col column where timestamp has date and/or datetime data that is in the format YYYY-mm-dd
#' @param date date as character string of length one in format YYYY-mm-dd
#' @return dataframe filtered for the date
#' @seealso 
#'  \code{\link[dplyr]{filter_all}},\code{\link[dplyr]{all_vars}}
#' @rdname filter_for_date
#' @export 
#' @importFrom dplyr filter_at all_vars %>%

filter_for_date <-
        function(.data, timestamp_col, date) {
                
                .Deprecated()
                timestamp_col <- enquo(timestamp_col)

                return(
                        .data %>%
                                dplyr::filter_at(vars(!!timestamp_col), dplyr::all_vars(grepl(date, .) == TRUE))
                )

        }





#' @title
#' Create Timestamp
#' @description 
#' (Deprecated) Adds a time delay in case an identifier is needed (do not want duplicate identifiers since multiple outputs can be created using the same exact date and time within a second)
#' @param add_sleep_time number of seconds to add between iterations.
#' @seealso 
#'  \code{\link[stringr]{str_remove}}
#' @rdname stamped
#' @export 
#' @importFrom stringr str_remove_all

stamped <- 
        function(string = FALSE, add_sleep_time = NULL) {
                
                .Deprecated(new = "datetime_to_col")
                
                 x <- as.character(Sys.time())
                 
                 if (!is.null(add_sleep_time)) {
                         Sys.sleep(add_sleep_time)
                 }
                
                if (string) {
                        stringr::str_remove_all(x,
                                                pattern = "[[:punct:]]| ")
                } else {
                        return(x)
                }
        }





