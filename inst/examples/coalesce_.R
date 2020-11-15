library(tidyverse)
test_data <- 
        tibble(A = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
              B = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
              C = sample(c(NA_integer_, 7:9), size = 10, replace = TRUE))

# Coalesce at an existing column
coalesce_at(test_data,
            col = A,
            B, 
            C,
            remove = FALSE)

coalesce_at(test_data,
            col = A,
            B, 
            C,
            remove = TRUE)


# Coalesce to a new column
coalesce_to(
        test_data,
        col = A2,
        B, 
        C,
        remove = FALSE
)

coalesce_to(
        test_data,
        col = A2,
        B, 
        C,
        remove = TRUE
)
