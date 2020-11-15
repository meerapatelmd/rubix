# Load Data
data(mtcars)

# To List of Vectors
cols_to_list(data = mtcars)


cols_to_list(data = mtcars,
             vs, am, gear)


# List to Dataframe
mtcars_list <- cols_to_list(data = mtcars)
list_to_tibble(mtcars_list)
