library(tidyverse)

test_data <-
        tibble(A = rep(NA_character_, 5),
               B = LETTERS[1:5],
               C = 1:5,
               D = rep("Z", 5))

test_data
deselect_if_all_na(test_data)
deselect_if_all_same(test_data)
