# Introduction to R
  
# Outline:
#   Setting up data
#   Exploring data
#   Editing data
#   Estimating regressions
#   Exporting regression output

# Data files: 
#   wage1.csv

# Setting up data ---------------------------------------------------------
rm(list = ls()) # clear the workspace

# Update pre-installed packages
# update.packages()

# Install packages
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("magrittr", dependencies = TRUE)
# install.packages("stargazer", dependencies = TRUE)

# Load packages
library(ggplot2)
library(tidyverse)
library(magrittr)
library(stargazer)

# Set directory
directory <- "C:/Programming/R/Econometrics-with-R/0. Introduction to R/"

# Get and set working directory
getwd()
setwd("C:/Programming/R/Econometrics-with-R/0. Introduction to R/")

# Read dataset
data <- read.csv("C:/Programming/R/Econometrics-with-R/0. Introduction to R/wage1.csv")
data <- read.csv(paste0(directory, "wage1.csv"))

# Exploring data ----------------------------------------------------------
  
# Describe data
str(data) # data structure
data %>% select(wage, educ, exper) %>% str
# The pipe `%>%` is a function that passes an object on the left-hand side 
# to the function on the right-hand side as the first argument. For details, type ?`%>%`.
# "data %>% select(wage, educ, exper)" is equivalent to "select(data, wage, educ, exper)".

# List data
data %>% select(wage, educ, exper) %>% head(10)

# Summary statistics 
data %>% stargazer(type = "text")
data %>% select(wage, educ, exper) %>% stargazer(type = "text")

# Summary statistics by group
data %>% select(female) %>% table
data %>% filter(female == 1) %>% stargazer(type = "text")
split(data, data$female) %>% walk( ~ stargazer(., type = "text"))
# 'split' divides a dataframe based on a factor variable, in this case 'female'.
# 'walk' applies the 'stargazer' function to each dataframe.
# '.' in the stargazer function stands for each dataframe.


# Editing data ------------------------------------------------------------
# Keep and drop variables
data %<>% select(wage, educ, exper, tenure, female, south, west)
data %<>% select(-tenure)
data %<>% filter(wage < 2)
# '%<>%' is a variant of the pipe function that assigns the result of the code 
# to the left-hand side object. 
# "data %<>% filter(wage < 2)" is equivalent to "data <- filter(data, wage < 2)"

# Generate new variables
data %<>% mutate(logwage = log(wage), 
                 educsq = educ^2, 
                 southwest = south + west)


# Estimating regressions and exporting regression output -------------------
# Use the package 'stargazer' which is already loaded.

# Estimate a simple regression model
linear.model.1 <- lm(wage ~ educ, data) # wage regressed on a constant and educ
summary(linear.model.1)

# Export results in table
stargazer(linear.model.1, type = "html", out = "regtable.html")
# 'stargazer' produces html file or Word file.
# The output is exported to the current working directory.

# Estimate a multiple regression model
linear.model.2 <- lm(wage ~ educ + exper, data)
summary(linear.model.2)

# Export results in table
stargazer(linear.model.1, linear.model.2,
          type = "html",
          out = "regtable.doc")
