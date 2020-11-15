library(tidyverse)
test_data <- 
        tibble(A = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
               B = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
               C = sample(c(NA_integer_, 7:9), size = 10, replace = TRUE),
               D = sample(c(NA_integer_, 10:12), size = 10, replace = TRUE),
               E = sample(c(NA_integer_, 13:15), size = 10, replace = TRUE),
               )

# Only operates on character columns
mutate_all_na_character(data = test_data)


test_data <- 
        tibble(A = sample(c(NA_character_, "NA", " ", "   ", "", 1:3), size = 10, replace = TRUE),
               B = sample(c(NA_character_, "NA", " ", "   ", "", 4:6), size = 10, replace = TRUE),
               C = sample(c(NA_character_, "NA", " ", "   ", "", 7:9), size = 10, replace = TRUE)
        )

# Results with all input character columns
mutate_all_na_character(data = test_data)

mutate_all_na_character(data = test_data,
                        blank = FALSE)

mutate_all_na_character(data = test_data,
                        blank = FALSE,
                        empty = FALSE)

mutate_all_na_character(data = test_data,
                        na_str = FALSE)