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
