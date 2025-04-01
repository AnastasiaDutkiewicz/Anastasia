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

# Data 1a
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

# Data 1b (stemming)
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
