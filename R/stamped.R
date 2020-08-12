#' @title
#' Create Timestamp
#' @description 
#' Adds a time delay in case an identifier is needed (do not want duplicate identifiers since multiple outputs can be created using the same exact date and time within a second)
#' @param add_sleep_time number of seconds to add between iterations.
#' @seealso 
#'  \code{\link[stringr]{str_remove}}
#' @rdname stamped
#' @export 
#' @importFrom stringr str_remove_all

stamped <- 
        function(string = FALSE, add_sleep_time = NULL) {
                
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