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

# create an object "cred" that will save the authenticated object that we can use for later sessions

consumer_key <- "ADD"
consumer_secret <- "ADD"
access_token <- "ADD"
access_secret <- "ADD"

cred <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Search for tweets 

search.string <- "artificial + intelligence" 
no.of.tweets <- 2000

tweets <- searchTwitter(search.string, n = no.of.tweets, lang = "en")
class(tweets)
tweets <- gsub("@\\w+", "", tweets)
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

tweets.df %>%
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

tweets.df %>%
  filter(freq > 15) %>%
  letterCloud(word = "AI", size = 1, color = "black")

# Sentiment analysis with tidytext - How do people feel about AI in these tweets?
# Tokenise tweets into single words

tweets.tokens <- unnest_tokens(tweets.df, term, word) ##UNFINISHED
