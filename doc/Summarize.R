## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rubix)
library(cave)
data("mtcars")

## ----summarize_variables function---------------------------------------------
rubix::summarize_variables(mtcars)

## -----------------------------------------------------------------------------
rubix::summarize_variables(mtcars, mpg, cyl)

## ----summarize_numeric_var----------------------------------------------------
rubix::summarize_numeric_vars(mtcars)

## ----summarize_numeric_var2---------------------------------------------------
rubix::summarize_numeric_vars(mtcars, cyl, disp)

## ----summarize_numeric_var2_error---------------------------------------------
mtcars2 <- mtcars
mtcars2$cyl <- as.character(mtcars2$cyl)
rubix::summarize_numeric_vars(mtcars2, cyl, disp)

## -----------------------------------------------------------------------------
rubix::summarize_values(mtcars)

