# Load Data  
data("mtcars")

arrange_int(mtcars, col = carb) %>%
        head()
arrange_int(mtcars, col = carb, desc = TRUE) %>%
        head()


mtcars %>%
mutate_all(as.character) %>%
arrange_int(carb)
