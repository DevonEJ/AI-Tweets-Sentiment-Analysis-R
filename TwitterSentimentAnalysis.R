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
library(gridExtra)

# Create Twitter API authentication object - add your own API credentials here 

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
#save(tweets, file = "aitweets.RData") # Save those same tweets for reproducability if desired, or refresh each time code runs

################################################
# Process tweets data to make usable for analysis

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

#############################################################
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

###########################################################
# 'Letter cloud' of frequent (> 15 freq) terms in shape 'AI'

tweets.plot %>%
  filter(freq > 15) %>%
  letterCloud(word = "AI", size = 1, color = "black")

##################################################################################
## Sentiment analysis with tidytext - How do people feel about AI in these tweets?

# Join the words with NRC sentiment lexicon

all_tweets <- tweets.plot %>%
  mutate(word = term) %>%
  left_join(get_sentiments("nrc")) %>%
  group_by(word) %>%
  filter(sentiment != "NA") %>%
  ungroup()

# Plot overall sentiment counts for AI tweets

tweet_plot <- ggplot(data = all_tweets, aes(x = as.factor(sentiment), y = freq, fill = sentiment)) +
  geom_bar(stat = "identity") +
  xlab("Sentiment") +
  ylab("Frequency") +
  ggtitle("Sentiment Profile of All Tweets") +
  theme(legend.position = "none") +
  coord_flip() 

# Plot top 10 words classified 'positive' sentiment in AI tweets

negative <- all_tweets %>%
  filter(sentiment == "positive") %>%
  group_by(word) %>%
  arrange(as.integer(desc(freq))) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(x = word, y = freq)) +
  geom_bar(stat = "identity", fill = "green", colour = "white") +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 10 Positive Words in AI Tweets") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70), limits = c(0, 70)) +
  coord_flip()
  
# Plot top 10 words classified 'negative' sentiment in AI tweets

positive <- all_tweets %>%
  filter(sentiment == "negative") %>%
  group_by(word) %>%
  arrange(as.integer(desc(freq))) %>%
  head(10) %>%
  ungroup() %>%
  ggplot(aes(x = word, y = freq)) +
  geom_bar(stat = "identity", fill = "red", colour = "white") +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 10 Negative Words in AI Tweets") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70), limits = c(0, 70)) +
  coord_flip()

# Let's compare those

grid.arrange(positive, negative)

###############################################################################################
## Are the retweeted AI tweets more positive or negative than a same-sized sample of the non-retweeted?

sample_size <-nrow(all_tweets)/4

# Get your sample of retweets first
retweets <- tweets.df %>%
  filter(retweetCount >= 1)

retweets_samp <- retweets[sample(nrow(retweets), sample_size),]

# Get a sample of the least retweeted AI tweets for comparison
no_retweets <- tweets.df %>%
  anti_join(retweets, by = "id") 

no_retweets_samp <- no_retweets[sample(nrow(no_retweets), sample_size),]
  
# Apply processing steps for retweets sample 
retweets_docs <- VCorpus(VectorSource(retweets_samp$text))

retweets_clean <- tm_map(retweets_docs, content_transformer(removeNumPunct))
retweets_clean <- tm_map(retweets_clean, content_transformer(removeURL))
retweets_clean <- tm_map(retweets_clean, removePunctuation)
retweets_clean <- tm_map(retweets_clean, removeNumbers)
retweets_clean <- tm_map(retweets_clean, content_transformer(tolower))
retweets_clean <- tm_map(retweets_clean, removeWords, c(stopwords("english"), "via", "amp", "ai", "artificial", "intelligence", "rt", "artificialintelligence", "will", "ways"))
retweets_clean <- tm_map(retweets_clean, stripWhitespace)

retweet.tdm <- TermDocumentMatrix(retweets_clean, control = list(wordLengths = c(1, Inf)))
retweet.terms <- rowSums(as.matrix(retweet.tdm))
retweets.plot <- data.frame(term = names(retweet.terms), freq = retweet.terms)

# Apply processing steps for non-retweets sample
no_retweet_docs <- VCorpus(VectorSource(no_retweets_samp$text))

no_retweet_clean <- tm_map(no_retweet_docs, content_transformer(removeNumPunct))
no_retweet_clean <- tm_map(no_retweet_clean, content_transformer(removeURL))
no_retweet_clean <- tm_map(no_retweet_clean, removePunctuation)
no_retweet_clean <- tm_map(no_retweet_clean, removeNumbers)
no_retweet_clean <- tm_map(no_retweet_clean, content_transformer(tolower))
no_retweet_clean <- tm_map(no_retweet_clean, removeWords, c(stopwords("english"), "via", "amp", "ai", "artificial", "intelligence", "rt", "artificialintelligence", "will", "ways"))
no_retweet_clean <- tm_map(no_retweet_clean, stripWhitespace)

no.retweet.tdm <- TermDocumentMatrix(no_retweet_clean, control = list(wordLengths = c(1, Inf)))
no.retweet.terms <- rowSums(as.matrix(no.retweet.tdm)) 
no.retweets.plot <- data.frame(term = names(no.retweet.terms), freq = no.retweet.terms)

# Create plots of the retweeted and non-retweeted samples of AI tweets 

retweet_plot <- retweets.plot %>% 
  mutate(word = term) %>%
  left_join(get_sentiments("nrc")) %>%
  group_by(word) %>%
  filter(sentiment != "NA") %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(sentiment), y = freq, fill = sentiment)) +
  geom_bar(stat = "identity") +
  xlab("Sentiment") +
  ylab("Frequency") +
  ggtitle("Sentiment Profile of Retweeted Tweets") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200), limits = c(0, 200)) +
  coord_flip() 

no_retweet_plot <- no.retweets.plot %>%
  mutate(word = term) %>%
  left_join(get_sentiments("nrc")) %>%
  group_by(word) %>%
  filter(sentiment != "NA") %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(sentiment), y = freq, fill = sentiment)) +
  geom_bar(stat = "identity") +
  xlab("Sentiment") +
  ylab("Frequency") +
  ggtitle("Sentiment Profile of Non-Retweeted Tweets") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200), limits = c(0, 200)) +
  coord_flip()

# Compare plots of all tweets with the most retweeted ones

grid.arrange(no_retweet_plot, retweet_plot)


