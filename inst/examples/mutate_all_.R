library(tidyverse)
test_data <- 
        tibble(A = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
               B = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
               C = sample(c(" apple ", "banana ", "    orange", NA_character_), size = 10, replace = TRUE))

# Mutate all to character
mutate_all_char(test_data)


# Trim whitespace at all character cols 
mutate_all_trimws(test_data,
                  which = "both")

mutate_all_trimws(test_data,
                  which = "left")

mutate_all_trimws(test_data,
                  which = "right")
