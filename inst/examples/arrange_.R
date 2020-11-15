library(tidyverse)
data("mtcars")

# As Numeric
arrange_num(mtcars, col = carb) 
arrange_num(mtcars, col = carb, desc = TRUE)

# As Integer
arrange_int(mtcars, col = carb) 

# As Double
arrange_dbl(mtcars, col = carb) 


# Native class is maintained
mtcars %>%
        mutate_all(as.character) %>%
        arrange_num(carb)

mtcars %>%
        mutate_all(as.character) %>%
        arrange_num(carb) %>%
        select(carb) %>%
        unlist() %>%
        class()
