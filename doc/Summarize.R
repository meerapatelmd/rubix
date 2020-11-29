## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rubix)
data("mtcars")

## ----summarize_variables function---------------------------------------------
summarize_variables(mtcars,
                    incl_num_calc = FALSE)

## ----summarize_numeric_var----------------------------------------------------
summarize_variables(data = mtcars,
                    incl_num_calc = TRUE)

## -----------------------------------------------------------------------------
value_count(data = mtcars)

