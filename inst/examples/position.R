library(tidyverse)
test_data <- 
        tibble(A = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
               B = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
               C = sample(c(NA_integer_, 7:9), size = 10, replace = TRUE),
               D = sample(c(NA_integer_, 10:12), size = 10, replace = TRUE),
               E = sample(c(NA_integer_, 13:15), size = 10, replace = TRUE),
               )


# To Position 1
to_position_1(data = test_data,
              col = D)

# To Last Position
to_last_position(data = test_data,
                 col = B)


# To Last Position
to_position_n(data = test_data,
              col = B,
              n = 3)

to_position_n(data = test_data,
              col = B,
              n = 4)

to_position_n(data = test_data,
              col = B,
              n = 2)

