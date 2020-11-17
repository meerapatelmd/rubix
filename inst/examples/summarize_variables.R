library(tidyverse)
test_data <- 
        tibble(
                Group = sample(c("Apple", "Pear"), size = 10, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
                C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 10, replace = TRUE)
        )


summarize_variables(data = test_data,
                    incl_num_calc = FALSE)

summarize_variables(data = test_data,
                    incl_num_calc = TRUE)

