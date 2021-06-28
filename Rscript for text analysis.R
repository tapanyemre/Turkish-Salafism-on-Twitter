######## Text Analysis: Quanteda, TM, and Ggplot2 ##########3

############ Load Libraries ##########
library(quanteda)
library(quanteda.corpora)
library(quanteda.dictionaries)
library(readtext)
library(spacyr)
library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(SnowballC)
library(tidyverse)
library(tidytext)
library(data.table)
library(DT)
library(xtable)
library(stopwords)
library(topicmodels)

############Get the Data #####
#get the list of the data files
setwd("~/Desktop/Thesis/DATA_text and network/text_thesis/DATA/")
allfiles = list.files(pattern="*_data.csv")

#get the female data
data_female = lapply(female, read.csv)
data_female <- do_call_rbind(data_female)
tweet_female <- data_female %>% select(date, tweet)
c_female <- as.character(tweet_female$tweet)

###########Clean and Reprocess the Data#####
#make a corpus in tm
corp_female <- tm::VCorpus(tm::VectorSource(tweet_female$tweet))

#clean data
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corp_female <- tm_map(corp_female, toSpace, "/")
corp_female <- tm_map(corp_female, toSpace, "@")
corp_female <- tm_map(corp_female, toSpace, "\\|")
# Convert the text to lower case
corp_female <- tm_map(corp_female, content_transformer(tolower))
# Remove numbers
corp_female <- tm_map(corp_female, removeNumbers)
# Remove english and turkish common stopwords
corp_female <- tm_map(corp_female, removeWords, stopwords("english"))
corp_female <- tm_map(corp_female, removeWords, stopwords("tr", source = "stopwords-iso"))
# Remove your own stop word
corp_female <- tm_map(corp_female, removeWords, trstop) 
# Remove punctuations
corp_female <- tm_map(corp_female, removePunctuation)
# Eliminate extra white spaces
corp_female <- tm_map(corp_female, stripWhitespace)
# Text stemming
corp_female <- tm_map(corp_female, stemDocument)
#make a corpus in quanteda
corp_female_q <- corpus(corp_female)
corp_female_t <- tokens(corp_female_q)

####### Most Frequent Words and term Frequencies######
# one hundred most frequent words
dfm_female <- dfm(corp_female_q, remove_punct=TRUE)
top100_female <- textstat_frequency(dfm_female, n = 100)

# frequencies of "allah", "hanzala", "suriye", "cihad" - these are matrixes, not numerics
sorted_dfm_female_freqs_t <- topfeatures(dfm_female, n = nfeat(dfm_female))
sorted_dfm_female_freqs_t[c("kadin", "evlilik", "aile", "cihad")]

# term frequency ratios
sorted_dfm_female_freqs_t["erdogan"] / sorted_dfm_female_freqs_t["hanzala"]

sorted_dfm_female_rel_freqs_t <- sorted_dfm_female_freqs_t / sum(sorted_dfm_female_freqs_t) * 100

# by weighting the dfm directly
dfm_female_pct <- dfm_weight(dfm_female, scheme = "prop") * 100

#get bar plot word frequency
textstat_frequency(dfm_female_pct, n = 15) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")

textstat_frequency(dfm_female, n = 15) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")

####### Topic Modelling########
#LDA Topic Modelling
dfm_female <- dfm_female %>% dfm_trim(min_termfreq = 4, max_docfreq = 10)
#convert the file for topic modelling
LDA_dfm_female <- convert(dfm_female, to = "topicmodels") %>% 
  LDA(k = 20)
# get top five terms per topic as a list
list_lda_dfm_female <- get_terms(LDA_dfm_female, 5)

###### Key Word in Context-KWIC#####
kwic_suriye_female <- kwic(corp_female_t, pattern = "kadın")
kwic_suriye_female

#####Wordcloud####
set.seed(123)
textplot_wordcloud(dfm_female, max_words = 100)
set.seed(1234)
textplot_wordcloud(dfm_female, min_count = 6, max_words=100, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"))
### most used hashtags and hashtags
hashtag_female <-  textstat_frequency(dfm(corpus(tm::VCorpus(tm::VectorSource(data_female$hashtags)), ), remove_punct=TRUE), n=10)
username <- textstat_frequency(dfm(corpus(tm::VCorpus(tm::VectorSource(data_female$username)), ), remove_punct=TRUE), n=10)
mentions <- textstat_frequency(dfm(corpus(tm::VCorpus(tm::VectorSource(data_female$mentions)), ), remove_punct=TRUE), n=10)

### date range
date_range <- min(as.Date(data_female$date))



