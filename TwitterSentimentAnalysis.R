# Clear Environment

rm(list=ls())

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

# create an object "cred" that will save the authenticated object that we can use for later sessions


consumer_key <- "ADD"
consumer_secret <- "ADD"
access_token <- "ADD"
access_secret <- "ADD"

cred <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Search for tweets 

search.string <- "lloyds banking group"
no.of.tweets <- 325

tweets <- searchTwitter(search.string, n = no.of.tweets, lang="en")
tweets
class(tweets)

# Convert text to data frame, and then corpus of documents (1 tweet = 1 document)

tweets.df <- twListToDF(tweets) 
dim(tweets.df)

tweet_docs <- VCorpus(VectorSource(tweets.df$text))

# Process the data -  removing symbols, numbers, misleading words etc.

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) # Will remove URLs from tweets when called below

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) # Removes characters that aren't letters or spaces when called below

tweets_clean <- tm_map(tweets_clean, content_transformer(removeNumPunct))
tweets_clean <- tm_map(tweets_clean, removeWords, c(stopwords("english")))#, "via", "amp", "lloyds", "banking", "group", "RT", "bank", "m", "aside", "has", "to"))
tweets_clean <- tm_map(tweets_clean, content_transformer(removeURL))
tweets_clean <- tm_map(tweet_docs, removePunctuation)
tweets_clean <- tm_map(tweets_clean, removeNumbers)
tweets_clean <- tm_map(tweets_clean, stripWhitespace)



# Create Term Document Matrix from clean corpus

tweet.tdm <- TermDocumentMatrix(tweets_clean, control = list(wordLengths = c(1, Inf)))

tweet.tdm

# Investigate most frequent terms in tdm

freq.terms <- rowSums(as.matrix(tweet.tdm)) 
freq.terms

df.terms <- data.frame(term = names(freq.terms), freq = freq.terms)
head(df.terms)

# Visualise top terms with counts

df.terms %>%
  arrange(freq) %>%
  top_n(10) %>%
  ggplot(aes(x = term, y = freq)) +
  geom_bar(stat = "identity") +
  xlab("Terms") +
  ylab("Counts") +
  coord_flip()