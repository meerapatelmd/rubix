library(tidyverse)
test_data <- 
        tibble(
                Group = sample(c("Apple", "Pear", "Maple", "Evergreen"), size = 15, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 15, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 15, replace = TRUE),
                C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 15, replace = TRUE),
                D     = sample(c(NA, TRUE, FALSE), size = 15, replace = TRUE)
        ) %>%
        dplyr::mutate(E = B)


categorize_fruits_and_trees <- 
categorize_ff(Fruit = c("Apple", "Pear"),
              Tree  = c("Maple", "Evergreen"))

categorize_fruits_and_trees(data = test_data, 
                            col = Group)

test_data2 <- 
        tibble(
                Group2 = sample(c("Apple", "Pear", "Maple", "Evergreen"), size = 15, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 15, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 15, replace = TRUE),
                C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 15, replace = TRUE),
                D     = sample(c(NA, TRUE, FALSE), size = 15, replace = TRUE)
        ) %>%
        dplyr::mutate(E = B)

test_data3 <- 
        tibble(
                Group3 = sample(c("Apple", "Pear", "Maple", "Evergreen"), size = 15, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 15, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 15, replace = TRUE),
                C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 15, replace = TRUE),
                D     = sample(c(NA, TRUE, FALSE), size = 15, replace = TRUE)
        ) %>%
        dplyr::mutate(E = B)


categorize_fruits_and_trees(data = test_data2, 
                            col = Group2)

categorize_fruits_and_trees(data = test_data3, 
                            col = Group3)
