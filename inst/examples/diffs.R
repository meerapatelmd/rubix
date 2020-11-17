library(tidyverse)
test_data <- 
        tibble(
                Group = sample(c("Apple", "Pear"), size = 10, replace = TRUE),
                A     = sample(c(NA_integer_, 1:3), size = 10, replace = TRUE),
                B     = sample(c(NA_integer_, 4:6), size = 10, replace = TRUE),
                C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 10, replace = TRUE),
                D     = sample(c(NA, TRUE, FALSE), size = 10, replace = TRUE)
        ) %>%
        dplyr::mutate(E = B)
        

test_data2 <- dplyr::bind_rows(test_data[1:5,],
                               tibble(
                                               Group = sample(c("Apple", "Pear"), size = 5, replace = TRUE),
                                               A     = sample(c(NA_integer_, 1:3), size = 5, replace = TRUE),
                                               B     = sample(c(NA_integer_, 4:6), size = 5, replace = TRUE),
                                               C     = sample(c(NA_real_, seq(from = 6.01, to = 6.09, by = 0.01)), size = 5, replace = TRUE),
                                               D     = sample(c(NA, TRUE, FALSE), size = 5, replace = TRUE)
                                       ) %>%
                                       dplyr::mutate(E = B))


# Rows difference
setdiff_nrow(x = test_data,
             y = test_data2)

setdiff_nrow(x = test_data2,
             y = test_data)

# Setdiff with a Column Match
setdiff_col_match(x = test_data,
                  y = test_data2 %>%
                        dplyr::mutate(F = 1:10) %>%
                        dplyr::mutate(G = 11:20))

setdiff_col_match(x = test_data2 %>%
                          dplyr::mutate(F = 1:10) %>%
                          dplyr::mutate(G = 11:20),
                  y = test_data)

setdiff_col_match(x = test_data,
                  y = test_data2 %>%
                          rename(F = E))

setdiff_col_match(y = test_data,
                  x = test_data2 %>%
                          rename(F = E))

# Compare Columns
compare_cols(x = test_data,
             y = test_data2)

compare_cols(x = test_data,
                  y = test_data2 %>%
                          dplyr::mutate(F = 1:10) %>%
                          dplyr::mutate(G = 11:20))

compare_cols(x = test_data2 %>%
                          dplyr::mutate(F = 1:10) %>%
                          dplyr::mutate(G = 11:20),
                  y = test_data)


compare_cols(x = test_data,
                  y = test_data2 %>%
                          rename(F = E))

compare_cols(y = test_data,
                  x = test_data2 %>%
                          rename(F = E))
