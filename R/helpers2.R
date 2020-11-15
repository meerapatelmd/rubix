



all_is_na <- 
        function(vector) {
                
                
                all(is.na(vector))
                
        }


all_not_na <- 
        function(vector) {
                
                
                !all_is_na(vector)
                
        }



any_is_na <- 
        function(vector) {
                
                
                any(is.na(vector))
                
        }
