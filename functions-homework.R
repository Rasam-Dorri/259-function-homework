#PSYC 259 Homework 4 - Writing functions
#For full credit, answer at least 6/8 questions

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)

### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 
#Write a function called "limit_replace" that will replace values less than 2 or greater than 8 with NA
#Then, run the function on x and save the results to a new vector "x_replace" to show it worked

### Answer:
# Q1: limit_replace function
limit_replace <- function(vec) {
  # Replace values < 2 or > 8 with NA
  vec[vec < 2 | vec > 8] <- NA
  vec
}

# Test on x
x_replace <- limit_replace(x)
x_replace





### Question 2 ---------- 

#Make a new version of limit_replace that asks for arguments for a lower bounary and an upper boundary
  #so that they can be customized (instead of being hard coded as 2 and 8)
#Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

### Answer:
# Q2: limit_replace with user-specified boundaries
limit_replace2 <- function(vec, lower_bound, upper_bound) {
  vec[vec < lower_bound | vec > upper_bound] <- NA
  vec
}

# Test on y with boundaries 4 and 6
y_replace <- limit_replace2(y, 4, 6)
y_replace




### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
  #and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean. 
#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 
#Make num_of_SDs default to 1
#run the new function on x, y, and z with 1 SD

### Answer:
# Q3: plus_minus_SD function
plus_minus_SD <- function(vec, num_of_SDs = 1) {
  m <- mean(vec, na.rm = TRUE)
  s <- sd(vec, na.rm = TRUE)
  c(m - num_of_SDs * s, m + num_of_SDs * s)
}

# Show +/- 1 SD around each vector’s mean
plus_minus_SD(x)
plus_minus_SD(y)
plus_minus_SD(z)



### Question 4 ----------

#Write an another new version of limit_replace
#This time, make the upper and lower boundaries optional arguments
#If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
#Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"

### Answer:
# Q4: limit_replace that uses +/- 1 SD by default (if boundaries not given)
limit_replace3 <- function(vec, lower = NULL, upper = NULL) {
  
  # If user didn't specify lower and/or upper, use plus_minus_SD(vec, 1)
  if (is.null(lower) || is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)  # returns a vector [lower, upper]
    lower  <- bounds[1]
    upper  <- bounds[2]
  }
  
  vec[vec < lower | vec > upper] <- NA
  vec
}

# Apply to each column in ds
ds_replace <- ds %>%
  mutate(across(.cols = everything(), .fns = ~ limit_replace3(.x)))
ds_replace



### Question 5 ----------

#Add a "stopifnot" command to your limit_replace function to make sure it only runs on numeric variables
#Try running it on a non-numeric input (like "id") to make sure it gives you an error

### Answer:
# Q5: Modify limit_replace3 to ensure numeric input
limit_replace3 <- function(vec, lower = NULL, upper = NULL) {
  # Force error if vec is not numeric
  stopifnot(is.numeric(vec))
  
  if (is.null(lower) || is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)
    lower  <- bounds[1]
    upper  <- bounds[2]
  }
  
  vec[vec < lower | vec > upper] <- NA
  vec
}
#### I think I would uncomment the codes below if I see any errors
# Try on id (should give an error)
# limit_replace3(id)  # Uncomment to test; it will throw an error



### Question 6 ----------

#What other requirements on the input do you need to make the function work correctly?
#Add another stopifnot to enforce one more requirement

### Answer:
###Ensure the vector has length > 1
limit_replace3 <- function(vec, lower = NULL, upper = NULL) {
  stopifnot(is.numeric(vec))
  stopifnot(length(vec) > 1)      # newly added requirement
  
  if (is.null(lower) || is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)
    lower  <- bounds[1]
    upper  <- bounds[2]
  }
  
  vec[vec < lower | vec > upper] <- NA
  vec
}





#### Ensure lower < upper (if provided)
limit_replace3 <- function(vec, lower = NULL, upper = NULL) {
  stopifnot(is.numeric(vec))
  
  # If user gave both boundaries, stop if lower >= upper
  if (!is.null(lower) && !is.null(upper)) {
    stopifnot(lower < upper)
  }
  
  if (is.null(lower) || is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)
    lower  <- bounds[1]
    upper  <- bounds[2]
  }
  
  vec[vec < lower | vec > upper] <- NA
  vec
}


### Question 7 ----------

#Clear out your workspace and load the built-in diamonds dataset by running the lines below
#RUN THIS CODE
rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

#Save your two functions to an external file (or files) 
#Then, load your functions from the external files(s)
#Next, run your limit_replace function on all of the numeric columns in the new data set
#and drop any rows with NA, saving it to a new tibble named "ds_trimmed"

### Answer:
# Q7:

# 1) Clear workspace
rm(list = ls())

# 2) Load diamonds and store in ds_diamonds
library(tidyverse)
ds_diamonds <- diamonds

# -- Suppose we have saved our functions in a file called "my_functions.R".
# For example, you might have a file "my_functions.R" that looks like this:
#
#     plus_minus_SD <- function(vec, num_of_SDs = 1) {
#       ...
#     }
#
#     limit_replace3 <- function(vec, lower = NULL, upper = NULL) {
#       ...
#     }
#
# 3) Save your functions to external file (you only do this once). Example:
# writeLines(
#   'plus_minus_SD <- function(vec, num_of_SDs = 1) {
#      ...
#    }
#    limit_replace3 <- function(vec, lower = NULL, upper = NULL) {
#      ...
#    }',
#   con = "my_functions.R"
# )

# 4) Load them back into R
# source("my_functions.R")

# 5) Run limit_replace (limit_replace3) on all numeric columns
ds_trimmed <- ds_diamonds %>%
  mutate(across(where(is.numeric), ~ limit_replace3(.x))) %>%
  drop_na()

# ds_trimmed now contains the diamond rows that remain after out-of-bound
# numeric values have been replaced with NA and then those NAs dropped.
ds_trimmed







### Question 8 ----------

#The code below makes graphs of diamond price grouped by different variables
#Refactor it to make it more efficient using functions and/or iteration
#Don't worry about the order of the plots, just find a more efficient way to make all 6 plots
#Each cut (Premium/Ideal/Good) should have a plot with trimmed and untrimmed data
#The title of each plot should indicate which cut and whether it's all vs. trimmed data

ds_diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, all") + 
  theme_minimal()

ds_diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, all") +
  theme_minimal()

ds_diamonds %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, all") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, trimmed") + 
  theme_minimal()

ds_trimmed %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, trimmed") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, trimmed") +
  theme_minimal()





### More Efficient Approach:
# Define a small helper function
plot_cut_box <- function(data, cut_value, label_suffix) {
  data %>%
    filter(cut == cut_value) %>%
    ggplot(aes(x = clarity, y = price)) +
    geom_boxplot() +
    ggtitle(paste(cut_value, label_suffix)) +
    theme_minimal()
}

# Vector of the cuts we want
my_cuts <- c("Premium", "Ideal", "Good")

# Generate all 6 plots in a loop
for (cut_val in my_cuts) {
  # untrimmed
  print(plot_cut_box(ds_diamonds, cut_val, "all"))
  # trimmed
  print(plot_cut_box(ds_trimmed, cut_val, "trimmed"))
}



