library(pacman)

p_load(tidyverse, ggplot2, quanteda, tokenizers, tidytext, tm, stringi, ggrepel, factoextra, tibble, SnowballC, wordcloud, smacof, ggfortify, ggthemes, dplyr, syuzhet, sentimentr, grid, gridExtra)

BA_AirlineReviews <- read.csv("BA_AirlineReviews.csv")
data <- BA_AirlineReviews[, c(1,7, 16)] 

data <- na.omit(data)

data <- data %>%
  rename(
    ReviewNumber = X
  )

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
data2 <- data
if (run_slow_parts) { 
    for (j in 1:nrow(data2)) {
        stemmed_description <- data2[j,] |> 
                                unnest_tokens(word, ReviewBody, drop=FALSE, to_lower=TRUE) |> 
                                anti_join(stop_words) |> 
                                pull(word) |> 
                                wordStem(language = "english")
        data2[j, "ReviewBody"] <- paste(stemmed_description, collapse = " ")
    }
}

##Remove Infrequent words}
review_words <- data2 %>%
  unnest_tokens(word, ReviewBody)

counts <- review_words %>% 
  count(word, sort=TRUE) 

exclusion_list <- counts %>%   #infrequent words and BA vocabulary
  filter(n < 0.01 * nrow(data2) | word == "ba" | word == "british" | word == "airway")

if (run_slow_parts) {
    for (j in 1:nrow(data2)) {
        remove_word <- data2[j,] %>% 
                          unnest_tokens(word, ReviewBody, to_lower=TRUE) %>% 
                          anti_join(exclusion_list)
        data2[j, "ReviewBody"] <- paste(remove_word[,"word"], collapse = " ")
    }
} 

#Frequent words
frequent_review_words <- data2 %>%
  unnest_tokens(word, ReviewBody)

frequent_words_counts <- frequent_review_words %>% 
  count(word, sort=TRUE)
  
#####PCA

review_tdm <- data2 %>%
  unnest_tokens(word, ReviewBody) %>%
  count(word, ReviewNumber, sort = TRUE) %>%
  ungroup() %>%
  cast_tdm(word, ReviewNumber, n)

PCA_counts <- rowSums(as.matrix(review_tdm)) 
PCA_sortedcount <- PCA_counts%>% 
  sort(decreasing=TRUE)
nwords<-200
PCAsortednames <- names(PCA_sortedcount[1:nwords])

review_dtm <- t(review_tdm)

pca_results <- prcomp(review_dtm, scale = FALSE, rank. = 30)
fviz_screeplot(pca_results,ncp=30)

ncomp<-6

j <- 1
topwordslist <- abs(pca_results$rotation[, j]) %>% 
  sort(decreasing = TRUE) %>% 
  head(10)
topwords <- names(topwordslist) 

for (j in 2:ncomp) {
  topwordslist <- abs(pca_results$rotation[, j]) %>% sort(decreasing = TRUE) %>% head(10)
  topwords <- cbind(topwords, names(topwordslist)) 
}

topwords


axeslist <- c(1, 2)
fviz_pca_var(pca_results, axes=axeslist 
             ,geom.var = c("arrow", "text")
              ,col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#8687d1ff", "#ff72d8ff"), 
              repel = TRUE)

rawLoadings <- pca_results$rotation[PCAsortednames,1:ncomp] %*% 
  diag(pca_results$sdev, ncomp, ncomp)

rotated <- varimax(rawLoadings) 

pca_results$rotation <- rotated$loadings 

pca_results$x <- scale(pca_results$x[,1:ncomp]) %*% 
  rotated$rotmat 

j <- 1 
toplist <- abs(pca_results$rotation[, j]) %>% 
  sort(decreasing = TRUE) %>% 
  head(10)
topwords <- names(toplist) 

for (j in 2:ncomp) {
  toplist <- abs(pca_results$rotation[, j]) %>% sort(decreasing = TRUE) %>% head(10)
  topwords <- cbind(topwords, names(toplist)) 
}

pca_results$x <- pca_results$x[1:200,] 

topwords

axeslist <- c(1, 2)
fviz_pca_var(pca_results, axes=axeslist 
             ,geom.var = c("arrow", "text")
              ,col.var = "contrib", 
              gradient.cols = c("#94ff56ff", "#00AFBB", "#ff72d8ff"), 
              repel = TRUE)

axeslist <- c(3, 4)
fviz_pca_var(pca_results, axes=axeslist 
             ,geom.var = c("arrow", "text")
              ,col.var = "contrib", 
              gradient.cols = c("#94ff56ff", "#00AFBB", "#ff72d8ff"), 
              repel = TRUE)

axeslist <- c(5, 6)
fviz_pca_var(pca_results, axes=axeslist 
             ,geom.var = c("arrow", "text")
              ,col.var = "contrib", 
              gradient.cols = c("#94ff56ff", "#00AFBB", "#ff72d8ff"), 
              repel = TRUE)

axeslist=c(1,2)
fviz_pca_ind(pca_results, axes = axeslist,
             col.ind = "cos2", 
             gradient.cols = c("#94ff56ff", "#00AFBB", "#ff72d8ff"), 
             repel = FALSE)

selected <- BA_AirlineReviews %>% 
  filter(grepl("1437", X))

selected$ReviewBody

#####MDS
tokenized_reviews <- tokens(corpus(data2, docid_field = "ReviewNumber", text_field = "ReviewBody"))

co_occurrence_matrix <- fcm(x = tokenized_reviews, context = "document", count = "frequency", tri=FALSE)

reviews_dfm <- dfm(tokenized_reviews) 
MDS_counts <- colSums(as.matrix(reviews_dfm)) 
co_occurrence_matrix <- as.matrix(co_occurrence_matrix)
diag(co_occurrence_matrix) <- MDS_counts

MDS_sortedcount <- MDS_counts%>% 
  sort(decreasing=TRUE)
MDS_sortednames <- names(MDS_sortedcount)
nwords <- 200
MDS_subset_words <- as.matrix(MDS_sortedcount[1:nwords])

co_occurrence_matrix <- co_occurrence_matrix[MDS_sortednames[1:nwords], MDS_sortednames[1:nwords]]
co_occurrence_matrix[1:7,1:7]

distances <- sim2diss(co_occurrence_matrix, method = "cooccurrence") 
distances[1:20,1:7]

MDS_map <- smacofSym(distances)
ggplot(as.data.frame(MDS_map$conf), aes(D1, D2, label = rownames(MDS_map$conf))) +
     geom_text(check_overlap = TRUE) + theme_minimal(base_size = 15) + xlab('') + ylab('') +
     scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)







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



library(sentimentr)

# Sentiment analysis with amplification (3B)
sentiment_with_amp <- sentiment_by(data1$ReviewBody)

#Sentiment analysis ignoring amplification
sentiment_no_amp <- sentiment_by(data1$ReviewBody, amplifier.weight = 0)

#Compare the results
comparison <- data.frame(
  Sentiment_With_Amp = sentiment_with_amp$ave_sentiment,
  Sentiment_No_Amp = sentiment_no_amp$ave_sentiment
)

#Visualization
ggplot(comparison, aes(x = Sentiment_With_Amp, y = Sentiment_No_Amp)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Effect of Amplification on Sentiment Scores",
       x = "Sentiment With Amplification",
       y = "Sentiment Without Amplification")
