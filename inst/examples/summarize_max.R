library(tidyverse)
test_data <- 
        tibble(
                Group = sample(c("Apple", "Pear"), size = 10, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
                C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 10, replace = TRUE)
        )


# Checking that all numeric cols are captured
all_numeric_cols(data = test_data)

# Ungrouped Summary
summarize_max(test_data)
summarize_max(test_data, A)

# Grouped Summary
summarize_max(test_data,
              grouper = Group)

summarize_max(test_data,
              B,
              na.rm = TRUE,
              grouper = Group)

summarize_max(test_data,
              B,
              na.rm = FALSE,
              grouper = Group)
