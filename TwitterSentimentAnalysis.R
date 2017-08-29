# Clear Environment
save(list = ls(all.names = TRUE), file = ".RData", envir = .GlobalEnv)

# Set Wd
setwd("C:/Projects/Projects")

# Load packages

library(twitteR)
library(ROAuth)
library(httr)
library(rjson)
library(devtools)
library(base64enc)
library(streamR)
library(RCurl)
library(stringr)
library(httpuv)
library(tm)
library(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
library(wordcloud2)
library(RColorBrewer)
library(htmlwidgets)
library(topicmodels)

# create an object "cred" that will save the authenticated object that we can use for later sessions

consumer_key <- "xwr2EIziRyLVyj2IQJltSmbG9"
consumer_secret <- "d2Lle10EMEnMQJ4BEnitG2aQo2jvWtdSoOwOGbm8l2blceB5pJ"
access_token <- "897829237310226432-y2VoDcFrLXpGScJU9vb309omF6bu62S"
access_secret <- "Kk48sk9FNoDlYKxa1Bhe7lJkuY8wyl4WsY76mdMSE01ps"

cred <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Search for tweets 

search.string <- "artificial + intelligence" 
no.of.tweets <- 2000

tweets <- searchTwitter(search.string, n = no.of.tweets, lang = "en")
class(tweets)
#tweets <- gsub("@\\w+", "", tweets)
#save(tweets, file = "aitweets.RData") # Save those same tweets for reproducability if desired, or refresh each time code runs

# Convert text to data frame, and then corpus of documents (1 tweet = 1 document)

tweets.df <- twListToDF(tweets) 
dim(tweets.df)

tweets_docs <- VCorpus(VectorSource(tweets.df$text))

# Process the data -  removing symbols, numbers, misleading words etc.

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) # Will remove URLs from tweets when called below

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) # Removes characters that aren't letters or spaces when called below

tweets_clean <- tm_map(tweets_docs, content_transformer(removeNumPunct))
tweets_clean <- tm_map(tweets_clean, content_transformer(removeURL))
tweets_clean <- tm_map(tweets_clean, removePunctuation)
tweets_clean <- tm_map(tweets_clean, removeNumbers)
tweets_clean <- tm_map(tweets_clean, content_transformer(tolower))
tweets_clean <- tm_map(tweets_clean, removeWords, c(stopwords("english"), "via", "amp", "ai", "artificial", "intelligence", "rt", "artificialintelligence", "will", "ways"))
tweets_clean <- tm_map(tweets_clean, stripWhitespace)

# Create Term Document Matrix from clean corpus

tweet.tdm <- TermDocumentMatrix(tweets_clean, control = list(wordLengths = c(1, Inf)))

# Which are the top 5 terms? Visualise with ggplot 2 barchart

freq.terms <- rowSums(as.matrix(tweet.tdm)) # Convert the TDM into a matrix first, then data frame to plot 

tweets.plot <- data.frame(term = names(freq.terms), freq = freq.terms)

tweets.plot %>%
  arrange(freq) %>%
  top_n(5) %>%
  ggplot(aes(x = term, y = freq, fill = term)) +
  geom_bar(stat = "identity") +
  xlab("Terms") +
  ylab("Counts") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()

# 'Letter cloud' of frequent (> 15 freq) terms in shape 'AI'
# HTML object created

tweets.plot %>%
  filter(freq > 15) %>%
  letterCloud(word = "AI", size = 1, color = "black")

## Sentiment analysis with tidytext - How do people feel about AI in these tweets?

# Join the words with NRC sentiment lexicon

sentiment <- tweets.plot %>%
  mutate(word = term) %>%
  left_join(get_sentiments("nrc")) %>%
  group_by(word) %>%
  filter(sentiment != "NA") %>%
  ungroup()

# Plot overall sentiment counts for AI tweets

ggplot(data = sentiment, aes(x = as.factor(sentiment), y = freq, fill = sentiment)) +
  geom_bar(stat = "identity") +
  xlab("Sentiment") +
  ylab("Frequency") 

# Plot top 10 words classified 'positive' sentiment in AI tweets

sentiment %>%
  filter(sentiment == "positive") %>%
  group_by(word) %>%
  arrange(as.integer(desc(freq))) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(x = word, y = freq)) +
  geom_bar(stat = "identity", fill = "green", colour = "black") +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 10 Positive Words in AI Tweets") +
  coord_flip()
  
# Plot top 10 words classified 'negative' sentiment in AI tweets

sentiment %>%
  filter(sentiment == "negative") %>%
  group_by(word) %>%
  arrange(as.integer(desc(freq))) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(x = word, y = freq)) +
  geom_bar(stat = "identity", fill = "red", colour = "black") +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 10 Negative Words in AI Tweets") +
  coord_flip()
  



