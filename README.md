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


# Kajol Cleaning Code
# Without Stemming and With Punctuation Removal
data_pun <- data %>%
  mutate(ReviewBody = tolower(ReviewBody)) %>%  # Convert to lowercase
  mutate(ReviewBody = str_squish(ReviewBody))   # Remove extra spaces

# Completely Cleaned Version CHATGPT
#FULLY CLEANED
data_cleaned <- data %>%
  mutate(ReviewBody = tolower(ReviewBody)) %>%  # Convert to lowercase
  mutate(ReviewBody = str_replace_all(ReviewBody, ":( |-|o)*\\(", " SADSMILE ")) %>%  # Find :( or :-( or : ( or :o(
  mutate(ReviewBody = str_replace_all(ReviewBody, ":( |-|o)*\\)", " HAPPYSMILE ")) %>% # Find :) or :-) or : ) or :o)
  mutate(ReviewBody = str_replace_all(ReviewBody, "\\$ ?[0-9]*[\\.,]*[0-9]+", " DOLLARVALUE ")) %>% # Replace dollar values
  mutate(ReviewBody = str_replace_all(ReviewBody, "[0-9]*[\\.,]*[0-9]+", " NUMBER ")) %>%  # Replace all numbers
  mutate(ReviewBody = str_replace_all(ReviewBody, "([0-9]+:)*[0-9]+ *am", " TIME_AM ")) %>%  # Replace AM time
  mutate(ReviewBody = str_replace_all(ReviewBody, "([0-9]+:)*[0-9]+ *pm", " TIME_PM ")) %>%  # Replace PM time
  mutate(ReviewBody = str_replace_all(ReviewBody, "-+:-+", " TIME ")) %>%  # Replace general time
  mutate(ReviewBody = str_replace_all(ReviewBody, "&", " and ")) %>%  # Replace &
  mutate(ReviewBody = str_replace_all(ReviewBody, "-+", " ")) %>%  # Remove hyphens
  mutate(ReviewBody = removePunctuation(ReviewBody)) %>%  # Remove remaining punctuation
  mutate(ReviewBody = removeNumbers(ReviewBody)) %>%  # Remove any remaining numbers
  mutate(ReviewBody = str_squish(ReviewBody)) %>%  # Remove extra spaces
  unnest_tokens(word, ReviewBody) %>%  # Tokenization
  anti_join(stop_words, by = "word") %>%  # Remove stop words
  mutate(word = wordStem(word))  # Apply stemming

# Olivia stemming (chat)
# Apply stemming
words <- words |> 
  mutate(stemmed_word = wordStem(word, language = "en")) 

# Print the number of words
print(paste("Number of words:", nrow(words)))

# Count occurrences of stemmed words
counts <- words |> count(stemmed_word, sort=TRUE)

# Print the number of unique stemmed words
print(paste("Number of unique words:", nrow(counts)))

# View top 10 words
head(counts, 10)




#anas this does all at once but it is less clear to a human reader
#Remove punctuation and lowercase
data_clean <- data %>%
  mutate(ReviewBody = str_to_lower(ReviewBody),
         ReviewBody = str_replace_all(ReviewBody, "[[:punct:]]", ""))  #this adds spacing, lowecase , and gets rid of commas, points and so on

