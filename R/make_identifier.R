#' Make a unique id
#' @description This is an alternative to converting the timestamp into an integer to serve as an identifgier, which requires using packages such as gmp to manage within R and then adds the trouble of having to figure out how to manage it as a bigint in a database. Using DatabaseConnector's dbWriteTable function, identifiers are converted from bigz to character and this also makes it difficult to use this data for joins with OMOP vocabularies where the concept_ids are int and what homegrown identifiers are primary required for. To avoid the trouble of dealing with large integers, the timestamp is used and the "202" in the year "2020" is removed. This means that the identifier will be unique to this decade.
#' @import stringr
#' @export


make_identifier <- 
        function() {
                startingid <- stamped(string = TRUE, add_sleep_time = 1)
                startingid <- stringr::str_remove_all(startingid, "^202") %>% as.integer()
                return(startingid)
        }