library(tidyverse)
test_data <- 
        tibble(Fruits = 
                c("Apples and pears",
                  "Citrus – oranges, grapefruits, mandarins and limes",
                  "Stone fruit – nectarines, apricots, peaches and plums",
                  "Tropical and exotic – bananas and mangoes",
                  "Berries – strawberries, raspberries, blueberries, kiwifruit and passionfruit")
        )

# Arrange by Character Count
arrange_by_nchar(test_data,
                 col = Fruits)

arrange_by_nchar(test_data,
                 col = Fruits,
                 desc = TRUE)
