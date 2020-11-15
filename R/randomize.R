




#' @export

randomize <- 
        function(data) {
                
                data[
                sample(1:nrow(data),
                       size = nrow(data),
                       replace = FALSE),]
                
                
                
        }