# Author: Gabriel Battcock
# Collaborators: Mo Osman & Kevin Tang
# Date created: 26-March-2024
# Last edited: 26-March-2024

# Introduction to R

#libraries

library(dplyr)
library(readr)
library(haven)
library(here)
library(hrbrthemes)

# Some of you will be new to R, so we will start off pretty basic. Like STATA, we 
# need to read in our dataset. Unlike STATA, our data set will be stored in R as 
# something called an object. To create objects, we need to use the creation operator
# " <- ". This allows multiple objects or datasets to be used at the same time
# In R studio, you will be able to see what objects you
# have within your environment (i.e. data that you can use within your scipts and
# analysis). Let's create some data.

# R needs to know where your file is stored. It will assumed that it is stored in 
# the same folder/directory as the the script is in.

# this script is based on the training provided by MAPS team. The full training
# is available here: https://github.com/dzvoti/r4hces 

# create new data

food_name <- "Maize"
food_subname <- "Meal"

full_name <- paste(food_name, food_subname)

food_quantity_g <- 60
food_quantity_mg <- food_quantity_g*1000


# Create character vectors
food_name <- "Maize"
food_name2 <- "Maize"
food_name3 <- "Rice"

# Test equality
food_name == food_name2
food_name == food_name3

# test inequality

food_name != food_name2
food_name != food_name3



# Create numeric object
age <- c(18,19,20,1,2,3,4,5,65)
# Test whether age is greater than 18

for(item in age){
  if(item >= 18) {
    print("You are an adult")
  }else{
    print("You are not an adult")
  }
}

food_name == food_name2 & food_quantity_g >10


##### Data structures
# Create a vector of character values
food_names <-
  c("Rice",
    "Maize",
    "Beans",
    "Cassava",
    "Potatoes",
    "Sweet potatoes",
    "Wheat")

#Create a vector of numeric values
consumpution <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01)

# Create a vector of logical values
is_staple <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

# Create a vector of mixed values
mixture <- c(5.2, TRUE, "CA")


length(food_names)
food_names[8]


##### data frames

# Create a data frame
food_df <-
  data.frame(
    food_names = c(
      "Rice",
      "Maize",
      "Beans",
      "Cassava",
      "Potatoes",
      "Maize",
      "Wheat"
    ),
    consumption = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01),
    is_staple = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = TRUE
  )

# Print the data frame
print(food_df)
tail(food_df)

food_df <- data.frame(food_names, consumption, is_staple, stringsAsFactors = TRUE)


# Create a tibble
food_tb <- tibble::tibble(
  food_names = c(
    "Rice",
    "Maize",
    "Beans",
    "Cassava",
    "Potatoes",
    "Maize",
    "Wheat"
  ),
  consumption = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01),
  is_staple = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
)
# check which class they are in 
class(food_t)
class(food_tb)


#### vectors as factors
food_names_factor <- factor(food_names)
# Create a factor from a vector of character values
food_names_factor_2 <-
  factor(
    food_names,
    levels = c(
      "Beans",
      "Rice",
      "Maize",
      
      "Cassava",
      "Potatoes",
      "Sweet potatoes",
      "Wheat"
    )
  )

# Print the factor
print(food_names_factor_2)


food_tb <- food_tb %>% 
  dplyr::mutate(food_names = factor(food_names))





### create functions

# Define a function
multiply <- function(arg1, arg2, ...) {
  # Function body
  return_value <- arg1 * arg2
  # Return value
  return(return_value)
}


##### Read in the data ---------------------------------------------------------






