# Group 16
Text Analytics Assignment 1 

BA_AirlineReviews <- read.csv("BA_AirlineReviews.csv")
data <- BA_AirlineReviews[, c(2,7, 16)] 
data <- na.omit(data)

# found on forum chatt by anas
#1)b)
library(stringr)
library(dplyr)
# Remove punctuation and lowercase
data_clean <- data %>%
  mutate(ReviewBody = str_to_lower(ReviewBody),
         ReviewBody = str_replace_all(ReviewBody, "[[:punct:]]", ""))  #this adds spacing, lowecase , and gets rid of commas, points and so on
