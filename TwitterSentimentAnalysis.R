# Clear Environment

rm(list=ls())

# Set Wd
setwd("C:/Users/Devon/OneDrive/Documents/R Scripts/Projects")

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

#install.packages("base64enc")

#create an object "cred" that will save the authenticated object that we can use for later sessions


consumer_key <- "ADD"
consumer_secret <- "ADD"
access_token <- "ADD"
access_secret <- "ADD"

cred <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Search for tweets 

search.string <- "#bitcoin"
no.of.tweets <- 200

tweets <- searchTwitter(search.string, n = no.of.tweets, lang="en")
tweets