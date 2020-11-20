#' Mutate given cols into a mdy date
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate_at
#' @importFrom lubridate mdy
#' @keywords internal

mutate_as_mdy <-
        function(dataframe,
                 ...) {
                
                Args <- dplyr::enquos(...)
                dataframe %>%
                        dplyr::mutate_at(vars(!!!Args), lubridate::mdy)
        }





#' Mutate given columns to ymd date
#' @importFrom dplyr enquos
#' @importFrom dplyr mutate_at
#' @importFrom lubridate ymd
#' @keywords internal

mutate_as_ymd <-
        function(dataframe,
                 ...) {
                
                Args <- dplyr::enquos(...)
                dataframe %>%
                        dplyr::mutate_at(vars(!!!Args), lubridate::ymd)
        }










#' Get the sum of the rows base on the designated columns
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @keywords internal

mutate_rowsum <-
        function(dataframe, ...) {
                sum_vars <- enquos(...)
                
                sums <- vector()
                for (i in 1:nrow(dataframe)) {
                        x <- dataframe %>%
                                dplyr::select(!!!sum_vars)
                        sums[i] <- sum(as.numeric(x[i,]))
                }
                
                dataframe %>%
                        dplyr::mutate(rowsum = sums)
        }





#' Parse a Date Var
#' @description This function takes a character variable of dates in various formats and parses it using a battery of parsing functions.
#' @return list of dataframes for each parsed date variable along with the parsed result resulting from a coalesce of all date parsing options
#' @param origin_eval if TRUE, includes evaluation of the variables as dates with origins of 1900-01-01 and 1970-01-01. Since the evaluation of origin is temperamental (ie 91884 is parsed into a date in 1997 which would be difficult to weed out downstream since it is a realistic date), it is not coalesced with the other parsed columns and is reported separately in the output. 
#' @seealso 
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}},\code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{bind}}
#'  \code{\link[lubridate]{ymd}}
#' @rdname parse_date_var
#' @keywords internal 
#' @importFrom rlang as_name
#' @importFrom purrr map set_names map2
#' @importFrom dplyr select mutate bind_cols %>%
#' @importFrom lubridate ymd mdy ydm myd dmy dym

parse_date_var <-
        function(.data, ..., quiet = TRUE, origin_eval = FALSE) {
                # test_vector <- c("91884", "", NA, "091884", "09984", "090984", "09/18/1984", "1984-09-18", "9/18/84", "9/9/84", "9-9-84", "9-18-84", "12-18-84")
                # test_df <- tibble(test_v = test_vector)
                
                cols <- enquos(...)
                
                col_labels <- sapply(cols, rlang::as_name)
                
                output <-
                        cols %>%
                        purrr::map(function(x) .data %>%
                                           dplyr::select(!!x) %>%
                                           dplyr::mutate(ymd = lubridate::ymd(!!x, quiet = quiet),
                                                         mdy = lubridate::mdy(!!x, quiet = quiet),
                                                         ydm = lubridate::ydm(!!x, quiet = quiet),
                                                         myd = lubridate::myd(!!x, quiet = quiet),
                                                         dmy = lubridate::dmy(!!x, quiet = quiet),
                                                         dym = lubridate::dym(!!x, quiet = quiet))) %>%
                        purrr::set_names(col_labels)
                
                
                if (origin_eval) {
                        
                        output <-
                                output %>%
                                purrr::map2(cols, function(x,y) x %>%
                                                    dplyr::mutate(origin_19700101 = as.Date(suppressWarnings(as.double(!!y)), origin = "1970-01-01")) %>%
                                                    dplyr::mutate(origin_19000101 = as.Date(suppressWarnings(as.double(!!y)), origin = "1900-01-01")))
                        
                        
                }
                
                output <- 
                        output %>% 
                        purrr::map2(cols, function(x,y) x %>%
                                            dplyr::mutate(!!y := coalesce(ymd, mdy, ydm, myd, dmy, dym)) %>%
                                                dplyr::select(!!y, any_of(starts_with("origin"))) %>%
                                                rename_all_prefix("parsed_")) 
                
                output <-
                        output %>%
                        purrr::map2(cols, function(x,y) .data %>%
                                                                dplyr::select(!!y, any_of(starts_with("origin"))) %>%
                                                                dplyr::bind_cols(x))
                return(output)
                
        }





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
#' @keywords internal 
#' @importFrom dplyr enquo arrange %>%


arrange_as_integer <-
        function(.data, column, desc = FALSE) {
          
                .Deprecated(new = "arrange_int")
                
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
#' @keywords internal

cleanup_colnames <-
        function(dataframe) {
                .Deprecated(new = "format_colnames")
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
#' @keywords internal

cleanup_listnames <-
        function(list) {
                .Deprecated(new = "format_names")
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
#' @keywords internal 
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
#'@keywords internal

dated <- function(punct = FALSE) {
            .Deprecated()
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
#' @keywords internal

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
#' @keywords internal

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
#' @keywords internal 
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
#' @keywords internal 
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










#' @title 
#' Arrange by a Given Column as Integer
#' @description 
#' (Deprecated) Perform an arrange function call on a dataframe with the values of the target column as an integer class.
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
#' @keywords internal 
#' @importFrom dplyr enquo arrange %>%


arrange_as_integer <-
        function(.data, column, desc = FALSE) {
                
                .Deprecated(new = "arrange_as_int")
                
                column <- dplyr::enquo(column)
                

                if (desc == FALSE) {
                        .data %>%
                                dplyr::arrange(as.integer(!!column))
                } else {
                        .data %>%
                                dplyr::arrange(desc(as.integer(!!column)))
                }

        }





#' @title Bring Columns to the Front
#' @description  Bring a vector of columns to the front of a dataframe. 
#' @param .data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname bring_to_front
#' @keywords internal 
#' @importFrom dplyr enquos select everything %>%


bring_to_front <-
        function(.data, ...) {
                
                .Deprecated()
                cols <- dplyr::enquos(...)
                .data %>% 
                        dplyr::select(!!!cols, dplyr::everything())
        }






#' @title General Dataframe Cleanup
#' @description
#' Convert all columns to character class and trim all left and right white spaces.
#' @param dataframe input dataframe
#' @importFrom dplyr mutate_all
#' @importFrom dplyr %>% 
#' @keywords internal

call_mr_clean <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character) %>%
                dplyr::mutate_all(trimws, "both")
}






#' Filter for a row n
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @keywords internal

filter_row_n <-
        function(.data, n, invert = FALSE) {
                
                .Deprecated()
                if (invert) {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() != n)
                        

                } else {
                        
                        .data %>%
                                dplyr::filter(dplyr::row_number() == n)
                        
                        
                }

        }










#' Adds primary key to dataframe in position 1
#' @param pkey_column_name name of pkey column
#' @param starting_number optional starting number for the primary key
#' @param width_left_pad_with_zero integer of the number of leading zeros desired. NULL by default.
#' @param prefix prefix desired with starting_number + left padding with zeros (if not NULL)
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @keywords internal

mutate_primary_key <-
        function(dataframe,
                 pkey_column_name,
                 starting_number = NULL,
                 width_left_pad_with_zero = NULL,
                 prefix = NULL) {

                pkey_column_name <- enquo(pkey_column_name)

                if (is.null(starting_number)) {
                        x <-
                        dataframe %>%
                                dplyr::mutate(!!pkey_column_name := 1:dplyr::n()) %>%
                                dplyr::select(!!pkey_column_name, dplyr::everything())

                        if (!is.null(width_left_pad_with_zero)) {
                                x <- x %>%
                                        dplyr::mutate(!!pkey_column_name := stringr::str_pad(!!pkey_column_name, width = width_left_pad_with_zero, side = "left", pad = "0"))
                        } else {
                                x <- x
                        }
                } else {
                        if (is.null(prefix)) {
                                starting_number <- as.integer(starting_number)
                                row_num <- nrow(dataframe)
                                pkey_values <- starting_number:((starting_number+row_num)-1)

                                x <-
                                        dataframe %>%
                                        dplyr::mutate(!!pkey_column_name := pkey_values) %>%
                                        dplyr::select(!!pkey_column_name, dplyr::everything())

                                print("success")

                                if (!is.null(width_left_pad_with_zero)) {
                                        x <- x %>%
                                                dplyr::mutate_at(vars(!!pkey_column_name), stringr::str_pad, width = width_left_pad_with_zero, side = "left", pad = "0")
                                } else {
                                        x <- x
                                }
                        } else {
                                starting_number <- as.integer(starting_number)
                                row_num <- nrow(dataframe)
                                pkey_values <- starting_number:((starting_number+row_num)-1)

                                x <-
                                        dataframe %>%
                                        dplyr::mutate(!!pkey_column_name := pkey_values) %>%
                                        dplyr::select(!!pkey_column_name, dplyr::everything())

                                if (!is.null(width_left_pad_with_zero)) {
                                        x <- x %>%
                                                dplyr::mutate_at(vars(!!pkey_column_name), stringr::str_pad, width = width_left_pad_with_zero, side = "left", pad = "0")
                                } else {
                                        x <- x
                                }


                                x <-
                                        x %>%
                                        dplyr::mutate(!!pkey_column_name :=
                                                              paste0(prefix,
                                                                     x %>%
                                                                             dplyr::select(!!pkey_column_name) %>%
                                                                             unlist()
                                                              ))

                        }

                }
                return(x)

        }





#' Normalize a set of values to a true NA
#' @description This functions takes all the fields in a dataframe and replaces the "NA" string with NA_character_. If blanks is set to TRUE, blanks of character length of 0 are also replaced with NA_character_.
#' @import dplyr
#' @import stringr
#' @keywords internal



normalize_all_to_na <- 
        function(.data, blanks = TRUE) {
                
                .Deprecated()
                
                .data <- mutate_all_na_str_to_na(.data = .data)

                if (blanks) {
                        
                        .data <- 
                                mutate_all_blank_to_na(.data = .data)
                        
                }
                
                return(.data)
        }





#' Normalize a set of values to a true NA
#' @description 
#' This functions takes all the fields in a dataframe and replaces the "NA" string with NA_character_. If blanks is set to TRUE, blanks of character length of 0 are also replaced with NA_character_.
#' @seealso 
#'  \code{\link[dplyr]{mutate_all}}
#' @rdname normalize_at_to_na
#' @keywords internal 
#' @importFrom dplyr mutate_at %>%

normalize_at_to_na <- 
        function(.data, at_col, blanks = TRUE) {
                
                .Deprecated()
                
                str_replace_na_string <-
                        function(vector) {
                                vector[vector %in% c("NA")] <- NA
                                return(vector)
                        }
                
                str_replace_blank <-
                        function(vector) {
                                vector[vector %in% c("")] <- NA
                                return(vector)
                        }
                
                at_col <- enquo(at_col)
                
                .data <-
                .data %>%
                        dplyr::mutate_at(vars(!!at_col), str_replace_na_string) 
                
                if (blanks) {
                        
                        .data <- 
                                .data %>%
                                dplyr::mutate_at(vars(!!at_col), str_replace_blank)
                        
                }
                
                return(.data)
        }





#' Normalize NA values
#' @description convert all "NA" and true NAs to NA
#' @importFrom stringr str_replace_all
#' @importFrom dplyr mutate_all
#' @importFrom dplyr na_if
#' @keywords internal

normalize_na <-
        function(.data) {
                .data %>%
                        dplyr::mutate_all(stringr::str_replace_all, "^NA$", "") %>%
                        dplyr::mutate_all(dplyr::na_if, "")
        }





#' Sample a dataframe for n obs and returns full dataframe if n > nrow
#' @param n number of observations
#' @param ... additional arguments for dplyr::sample_n function
#' @importFrom dplyr sample_n
#' @importFrom dplyr distinct
#' @keywords internal

sample_n_or_bust <-
  function(dataframe, n, ...) {
    dplyr::sample_n(tbl = tbl, size = n, replace = TRUE, ...) %>%
                        dplyr::distinct()
  }





#' Send columns to back
#' @importFrom dplyr enquos
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @keywords internal

send_to_back <-
        function(.data, ...) {
                
                cols <- dplyr::enquos(...)
                
                .data_back <-
                .data %>% 
                        dplyr::select(!!!cols)
                
                .data_front <- 
                        .data %>% 
                        dplyr::select(-all_of(colnames(.data_back)))
                
                .data_front %>%
                        dplyr::bind_cols(.data_back)
        }















#' Filters a dataframe for today's date in a timestamp column
#' @param dataframe dataframe with timestamp column
#' @param timestamp_col column where timestamp has date and/or datetime data that is in the format YYYY-mm-dd
#' @return dataframe filtered for today's date
#' @import dplyr
#' @keywords internal

filter_for_today <-
        function(dataframe, timestamp_col) {
                timestamp_col <- enquo(timestamp_col)

                today <- as.character(Sys.Date())

                return(
                        dataframe %>%
                                dplyr::filter_at(vars(!!timestamp_col), dplyr::all_vars(grepl(today, .) == TRUE))
                )

        }





#' @title 
#' Aggregate Function
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}}
#' @rdname group_by_aggregate
#' @keywords internal 
#' @importFrom dplyr enquos enquo group_by summarize_at ungroup %>%

group_by_aggregate <- 
                function(dataframe, 
                         ...,
                         agg.col,
                         collapse = "|") {
                        
                        
                        .Deprecated()
                        
                                
                                        group_by_cols <- dplyr::enquos(...) 
                                        agg.col <- dplyr::enquo(agg.col)
                                        
                                        
                                        dataframe %>%
                                                dplyr::group_by(!!!group_by_cols) %>%
                                                dplyr::summarize_at(vars(!!agg.col), function(x) paste(x, collapse = collapse)) %>%
                                                dplyr::ungroup()
                
                
                
                }





#' Aggregate Function
#' @importFrom dplyr enquos
#' @importFrom dplyr enquo
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_at
#' @importFrom dplyr ungroup
#' @keywords internal

group_by_unique_aggregate <- 
                function(dataframe, 
                         ...,
                         agg.col,
                         collapse = "|") {
                        
                                
                                        group_by_cols <- dplyr::enquos(...) 
                                        agg.col <- dplyr::enquo(agg.col)
                                        
                                        
                                        dataframe %>%
                                                dplyr::group_by(!!!group_by_cols) %>%
                                                dplyr::summarize_at(vars(!!agg.col), function(x) paste(unique(x), collapse = collapse)) %>%
                                                dplyr::ungroup()
                
                
                
                }





#' Make a unique id
#' @description This is an alternative to converting the timestamp into an integer to serve as an identifgier, which requires using packages such as gmp to manage within R and then adds the trouble of having to figure out how to manage it as a bigint in a database. Using DatabaseConnector's dbWriteTable function, identifiers are converted from bigz to character and this also makes it difficult to use this data for joins with OMOP vocabularies where the concept_ids are int and what homegrown identifiers are primary required for. To avoid the trouble of dealing with large integers, the timestamp is used and the "202" in the year "2020" is removed. This means that the identifier will be unique to this decade.
#' @import stringr
#' @keywords internal


make_identifier <- 
        function() {
                startingid <- stamped(string = TRUE, add_sleep_time = 1)
                startingid <- stringr::str_remove_all(startingid, "^202") %>% as.integer()
                return(startingid)
        }





#' Adds a column if it doesn't exist in the dataframe
#' @param column_name character string of new column name
#' @param value character vector of length 1 of the new value. Defaults to "".
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @keywords internal

mutate_if_not_exist <-
        function(dataframe, column_name, value = "") {
                if (!(column_name %in% colnames(dataframe))) {
                        column_name <- enquo(column_name)

                        x <-
                                dataframe %>%
                                dplyr::mutate(`:=`(!!column_name, value))

                        return(x)
                } else {
                        return(dataframe)
                }

        }





#' Add timestamp to the entire dataframe at position 1 with the provided column name
#' @param new_col_name defaults to "TIMESTAMP"
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @keywords internal

mutate_timestamp_column <-
  function(dataframe, new_col_name = "TIMESTAMP") {
      new_col_name <- enquo(new_col_name)

      dataframe %>%
              mutate(!!new_col_name := as.character(Sys.time())) %>%
              select(!!new_col_name, everything())

  }















#' Filter for the first row
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @keywords internal

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
#' @keywords internal 
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
#' @keywords internal

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
#' @keywords internal

slice_first_row <-
        function(dataframe) {
                dataframe %>%
                        dplyr::slice(-1)
        }












#' General Dataframe Cleanup
#' Convert all columns to character class and trim all left and right white spaces.
#' @param dataframe input dataframe
#' @importFrom dplyr mutate_all
#' @keywords internal

mutate_all_as_char <- function(dataframe) {
        dataframe %>%
                dplyr::mutate_all(as.character)
}






#' Replace "" with <NA>
#' @return A tibble
#' @importFrom tibble as_tibble
#' @keywords internal

mutate_all_blank_to_na <- 
        function(.data) {
                .data <-
                        .data %>%
                        tibble::as_tibble()
                
                .data[.data == ""] <- NA_character_
                return(.data)
        }





#' Mutate all "NA" to <NA>
#' @return A tibble
#' @importFrom tibble as_tibble
#' @keywords internal

mutate_all_na_str_to_na <- 
        function(.data) {
                .data <-
                        .data %>%
                        tibble::as_tibble()
                
                .data[.data == "NA"] <- NA_character_
                return(.data)
        }





#' Substitute all true NA values in a dataframe as blank
#' @importFrom dplyr mutate_all
#' @keywords internal
mutate_all_na_to_blank <-
        function(dataframe, include_na_as_string = TRUE) {
                x <- dataframe
                x[is.na(x)] <- ""

                if (include_na_as_string == TRUE) {
                        x <- x %>%
                                dplyr::mutate_all(str_replace_all, "NA", "")
                } else {
                        x <- x
                }
                return(x)
        }










#' Mutate cols to integer data type
#' @import dplyr
#' @keywords internal

mutate_to_integer <- 
        function(.data, ...) {
                
                cols <- enquos(...)
                
                .data %>%
                        dplyr::mutate_at(vars(!!!cols), as.integer)
                
                
        }










#' Mutate a column to position 1
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr enquo
#' @keywords internal


mutate1 <-
        function(dataframe, ...) {
                col <- list(...)
                
                dataframe <- 
                        dataframe %>%
                        dplyr::mutate(...)

                 dataframe %>%
                         dplyr::select(!!names(col), everything())
        }






#' @title 
#' Convert a Vector to a Tibble 
#' @param vector        Vector that will become the column in the tibble
#' @param new_col       Name of the new column
#' @return
#' A tibble with 1 column with `new_col` as the name.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}}
#'  \code{\link[tibble]{tibble}}
#' @rdname vector_to_tibble
#' @keywords internal 
#' @importFrom dplyr enquo
#' @importFrom tibble tibble


vector_to_tibble <-
        function(vector, new_col) {
                .Deprecated(new = "as_tibble_col",
                            package = "tibble")
                new_col <- dplyr::enquo(new_col)
                tibble::tibble(!!new_col := vector)
        }




#' Summarize the counts of values in columns
#' @description (Deprecated) To group by more than 1 variable can be summarized using summarize_grouped_vars().
#' @param .data                 data frame      
#' @param ...                   group by variables     
#' @param names_to              passed to tidyr pivot_longer()
#' @param values_to             passed to tidyr pivot_longer()
#' @seealso 
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{summarise}}
#'  \code{\link[stringr]{str_replace}}
#' @seealso 
#'  \code{\link[rlang]{as_name}}
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{rename}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#'  \code{\link[tidyr]{pivot_longer}}
#' @rdname summarize_values
#' @keywords internal 
#' @importFrom rlang as_name
#' @importFrom dplyr mutate_at group_by_at summarise_at mutate rename select everything
#' @importFrom tidyr pivot_longer
#' @importFrom centipede no_na
#' @importFrom magrittr %>%

summarize_values <-
  function(.data,
           ...,
           names_to = "name",
           values_to = "value") {
    
    
    .Deprecated(new = "value_count")
    summaryFunLibrary <- 
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
    
    cols <- enquos(...)
    col_labels <- sapply(cols, rlang::as_name) 
    
    inverse_col_labels <- colnames(.data)[!(colnames(.data) %in% col_labels)]
    #print(inverse_col_labels)
    
    # .data %>% 
    #         dplyr::group_by_at(vars(!!!cols)) %>%
    #         dplyr::summarize_at(vars(!!summary_var),
    #                             ~length(.)) %>%
    #         dplyr::ungroup()
    #         
    
    .data %>%
      dplyr::mutate_at(vars(all_of(inverse_col_labels)), as.character) %>% 
      tidyr::pivot_longer(cols = all_of(inverse_col_labels),
                          names_to = names_to,
                          values_to = values_to) %>%
      dplyr::group_by_at(vars(c(!!!cols,
                                !!names_to))) %>%
      dplyr::summarise_at(vars(!!values_to),
                          summaryFunLibrary$categorical) %>%
      dplyr::mutate(NET_COUNT = COUNT-(NA_COUNT+NA_STR_COUNT+BLANK_COUNT)) %>%
      dplyr::rename(!!values_to := DISTINCT_VALUES) %>%
      dplyr::select(!!!cols,
                    !!names_to,
                    !!values_to,
                    NET_COUNT,
                    dplyr::everything())
    
    
    
  }



#' @title 
#' Summarize groups by count
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}}
#' @rdname summarize_grouped_n
#' @keywords internal 
#' @importFrom dplyr enquos group_by summarize arrange ungroup desc %>%


summarize_grouped_n <-
  function(dataframe,
           ...,
           desc = FALSE) {
    
    .Deprecated(new = "count",
                package = "dplyr")
    
    cols <- dplyr::enquos(...)
    
    if (desc == FALSE) {
      dataframe %>%
        dplyr::group_by(!!!cols) %>%
        dplyr::summarize(n = n()) %>%
        dplyr::arrange(n) %>%
        dplyr::ungroup()
    } else {
      dataframe %>%
        dplyr::group_by(!!!cols) %>%
        dplyr::summarize(n = n()) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::ungroup()
    }
    
  }




#' Summarize a variable by the standard summary functions
#' @param ... grouping vars. If missing, all variables will be summarized.
#' @seealso 
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate_all}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{pivot_wider}}
#' @rdname summarize_var_group
#' @keywords internal 
#' @importFrom dplyr enquos group_by_at summarize_all mutate_at %>%
#' @importFrom tidyr pivot_longer pivot_wider

summarize_var_group <-
  function(.data, ...) {
    
    
    .Deprecated(new = "summarize_variables")
    
    summary_functions <-
      list(
        COUNT = ~ length(.),
        DISTINCT_COUNT = ~ length(unique(.)),
        NA_COUNT = ~ length(.[is.na(.)]),
        NA_STR_COUNT = ~ length(.[. %in% c("NA", "#N/A", "NaN", "NAN")]),
        BLANK_COUNT = ~ length(.[. %in% c("")]),
        DISTINCT_VALUES = ~ paste(unique(as.character(.)), collapse="|") #,
        #DISTINCT_VALUES_EXPR = ~ cave::vector_to_string(.)
      )
    
    if (!missing(...)) {
      
      cols <- dplyr::enquos(...)
      
      output_1 <-
        .data %>% 
        dplyr::group_by_at(vars(!!!cols)) %>%
        dplyr::summarize_all(summary_functions) %>%
        bring_to_front(!!!cols)
      
      output_2 <- 
        output_1   %>%
        dplyr::mutate_at(vars(-group_cols()), as.character) %>%
        tidyr::pivot_longer(col = contains(names(summary_functions)),
                            names_pattern = paste0("(^.*?)[_](", paste(paste0(names(summary_functions), "$"), collapse = "|"), ")"),
                            names_to = c("Variable", "Parameter"),
                            values_to = "Value")
      
      output_3 <- 
        output_2  %>%
        tidyr::pivot_wider(id_cols = c(!!!cols, Variable),
                           names_from = Parameter,
                           values_from = Value) 
      
      
      
      return(output_3)
      
    } else {
      stop("group_cols() not provided. Use summarize_variables() for ungrouped summary.")
    }
  }












