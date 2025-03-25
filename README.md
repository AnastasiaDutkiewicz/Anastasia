# Group 16
Text Analytics Assignment 1 

BA_AirlineReviews <- read.csv("BA_AirlineReviews.csv")
data <- BA_AirlineReviews[, c(2,7, 16)] 
data <- na.omit(data)
