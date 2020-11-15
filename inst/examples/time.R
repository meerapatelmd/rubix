library(tidyverse)
test_data <- 
        tibble(Fruits = 
                       c("Apples and pears",
                         "Citrus – oranges, grapefruits, mandarins and limes",
                         "Stone fruit – nectarines, apricots, peaches and plums",
                         "Tropical and exotic – bananas and mangoes",
                         "Berries – strawberries, raspberries, blueberries, kiwifruit and passionfruit")
        )

# Datetime 
datetime_to_col(data = test_data,
                var = "test_datetime")


# Date 
date_to_col(data = test_data,
            var = "test_date")
