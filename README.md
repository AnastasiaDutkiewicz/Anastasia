library(pacman)

p_load(tidyverse, ggplot2, tokenizers, tidytext, tm, stringi, ggrepel, dplyr, wordcloud, syuzhet, SnowballC, sentimentr, grid, gridExtra)

BA_AirlineReviews <- read.csv("BA_AirlineReviews.csv")
data <- BA_AirlineReviews[, c(2,7, 16)] 

data <- na.omit(data)

# Clean data
data$ReviewBody <- as.character(data$ReviewBody)  %>% 
                            tolower() %>% 
                            {gsub(":( |-|o)*\\("," sadsmile ", .)} %>%     
                            {gsub(":( |-|o)*\\)"," happysmile ", .)} %>%  
                            {gsub("(\"| |\\$)-+\\.-+","number", .)} %>%    
                            {gsub("([0-9]+:)*[0-9]+ *am"," time_am", .)} %>%  
                            {gsub("([0-9]+:)*[0-9]+ *pm","time_pm", .)} %>% 
                            {gsub("-+:-+","time", .)} %>%                    
                            {gsub("\\$ ?[0-9]*[\\.,]*[0-9]+"," dollarvalue ", .)} %>%   
                            {gsub("[0-9]*[\\.,]*[0-9]+"," number ", .)} %>% 
                            {gsub("-"," ", .)} %>%                         
                            {gsub("&"," and ", .)} %>%                      
                            {gsub("\"+"," ", .)} %>%                    
                            {gsub("\\|+"," ", .)} %>%                      
                            {gsub("_+"," ", .)} %>%                        
                            {gsub(";+"," ", .)} %>%                      
                            {gsub(" +"," ", .)} %>%                         
  {gsub("\\.+","\\.", .)}

# Remove punctuation 
data$ReviewBody <- gsub("[[:punct:]]", "", data$ReviewBody)

# Data 1
run_slow_parts = TRUE
data1 <- data
if (run_slow_parts) { 
    for (j in 1:nrow(data1)) {
        remove_stopwords <- data1[j,] |> 
                                unnest_tokens(word, ReviewBody, drop=FALSE, to_lower=TRUE) |> 
                                anti_join(stop_words) |> 
                                pull(word)
        data1[j, "ReviewBody"] <- paste(remove_stopwords, collapse = " ")
    }
}

# Data 2 (stemming)
run_slow_parts = TRUE
data2.stemmed <- data
if (run_slow_parts) { 
    for (j in 1:nrow(data2.stemmed)) {
        stemmed_description <- data2.stemmed[j,] |> 
                                unnest_tokens(word, ReviewBody, drop=FALSE, to_lower=TRUE) |> 
                                anti_join(stop_words) |> 
                                pull(word) |> 
                                wordStem(language = "english")
        data2.stemmed[j, "ReviewBody"] <- paste(stemmed_description, collapse = " ")
    }
}







# 3A without stemming and including punctuation


####stemming

#Dictionary-Based Sentiment Analysis (SYUZHET)
library(syuzhet)

#Dictionary-based sentiment score
data1$syuzhet_score <- get_sentiment(data1$ReviewBody, method = "syuzhet")

#Categorize polarity
data1$sentiment_cat <- case_when(
  data1$syuzhet_score > 0 ~ "Positive",
  data1$syuzhet_score < 0 ~ "Negative",)

#Contingency table against "Recommended"
table(data1$Recommended, data1$sentiment_cat)

#Visualization
ggplot(data1, aes(x = Recommended, fill = sentiment_cat)) +
  geom_bar(position = "fill") +
  labs(title = "Dictionary Sentiment vs Recommendation", y = "Proportion", x = "Recommended") +
  theme_minimal()
