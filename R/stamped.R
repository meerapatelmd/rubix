#' Create Timestamp
#' @description Adds a 1 sec delay in case an identifier is needed
#' @import stringr
#' @export

stamped <- 
        function(string = FALSE) {
                 x <- as.character(Sys.time())
                 Sys.sleep(1)
                if (string) {
                        stringr::str_remove_all(x,
                                                pattern = "[[:punct:]]| ")
                } else {
                        return(x)
                }
        }