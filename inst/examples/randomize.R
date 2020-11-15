library(tidyverse)
test_data <- 
        tibble(
                Group = sample(c("Apple", "Pear"), size = 10, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE)
        )


randomize(test_data)

