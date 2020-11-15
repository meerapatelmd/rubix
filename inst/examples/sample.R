library(tidyverse)
test_data <- 
        tibble(
                Group = sample(c("Apple", "Pear"), size = 10, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE)
        )


# Sample less than row count
sample_all_or_n(data = test_data,
                n = 5)

# When n is greater than row count, the dataframe is returned unchanged
sample_all_or_n(data = test_data,
                n = 11)
