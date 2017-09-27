#
#  Explore UN debates dataset
#
#  Dataset downloaded from Kaggle Datasets
#  https://www.kaggle.com/unitednations/un-general-debates
#
#

library(dplyr)
library(tidyr)
library(tm)
library(topicmodels)
library(ggplot2)
library(ggthemes)
library(tidytext)

# load data
df = read.csv("Data/un-general-debates.csv", stringsAsFactors = FALSE)
# read country codes
cntryCodes = read.csv("Data/cntryCodes.csv", stringsAsFactors = FALSE)

df = df %>% rename(cntryabb = country)
cntryCodes = cntryCodes %>% rename(cntryabb = iso_alpha_3_code) %>% select(cntryabb, country)

# check if all cntryabb in UN dataset are matched to name
unique(df$cntryabb)[!(unique(df$cntryabb) %in% cntryCodes$cntryabb)]

# Manually add the country names for the unmatched country codes
cntryCodesMissing = data.frame(
  cntryabb = c("YDYE", "CSK", "YUG", "DDR", "EU"),
  country = c("Yemen", "Czhechoslovakia", "Serbia and Montenegro", 
              "German Democratic Republic", "Europe"),
  stringsAsFactors = FALSE
)

cntryCodes = bind_rows(cntryCodes, cntryCodesMissing)

df2 = inner_join(df, cntryCodes, by = "cntryabb")
df2 = df2 %>% arrange(year, cntryabb)
df2$docid = seq(1, nrow(df2))
head(df2)

# Number of countries in the data
length(unique(df2$country))

# count number of countries presented at each session
session_cntrycnt = df2 %>% group_by(year, country) %>% group_by(year) %>% 
                   summarize(cnt = n())
ggplot() + geom_line(data = session_cntrycnt, aes(x = year, y = cnt)) + 
  xlab("") + ylab("# Countries") + expand_limits(y = 0) + theme_bw()

# get text in tidy format
tidy_df2 = df2 %>% unnest_tokens(word, text)
head(tidy_df2)

# remove stopwords
data("stop_words")
cleaned_df2 = anti_join(tidy_df2, stop_words) %>% arrange(docid)
head(cleaned_df2)

# remove numbers
chknumeric = is.na(as.numeric(cleaned_df2$word))
cleaned_df2 = cleaned_df2[chknumeric,]

# get word counts for each document
word_doc_cnt = cleaned_df2 %>% group_by(docid, year, country, word) %>% 
                summarize(wordcnt = n())
head(word_doc_cnt)

# number of documents in which a word appears
totdocs = nrow(df2)
word_freq = word_doc_cnt %>% group_by(word) %>% 
                summarize(totwordcnt = sum(wordcnt), docfreq = n()) %>%
                  mutate(idf = log(totdocs/docfreq), tfidf = totwordcnt * idf, docfreqpct = docfreq/totdocs)

numwords = table(cut(word_freq$docfreqpct, breaks = seq(0, 1, 0.1)))
numwords

word_freq %>% filter(docfreqpct >= 0.6) %>% arrange(desc(totwordcnt)) %>% print(n = Inf)
word_freq %>% arrange(desc(tfidf))

# Remove words that are in either less than 5% of talks or in more than 90% of talks
word_freqfilt = word_freq %>% filter(docfreqpct >= 0.05, docfreqpct <= 0.6)

word_doc_cntfilt = inner_join(word_doc_cnt, word_freqfilt[,c("word", "tfidf")], by = "word")


# Create year buckets
yrbucket = cut(word_doc_cntfilt$year, breaks = c(1970, 1980, 1990, 2000, 2010, 2017), 
                 labels = c("70s", "80s", "90s", "2000s", "2010s"), include.lowest = TRUE)
word_doc_cntfilt$yrbucket = yrbucket

# check
word_doc_cntfilt %>% group_by(yrbucket) %>% summarize(minyr = min(year), maxyr = max(year)) %>% print(n = Inf)

# Top 10 words by year
top10words = word_doc_cntfilt %>% group_by(yrbucket, word) %>% summarize(wordcnt = sum(wordcnt)) %>% 
    top_n(n = 10, wordcnt) %>% arrange(yrbucket, desc(wordcnt))

top10words = top10words %>% group_by(yrbucket) %>% mutate(x = 1, y = n():1)
ggplot() + geom_text(data = top10words, aes(x = x, y = y, label = word)) + 
           facet_grid(~yrbucket) + theme_void(20)

# Top 10 words by year for a country
pickCountry = "Canada"
top10cntrywords = word_doc_cntfilt %>% filter(country == pickCountry) %>% group_by(yrbucket, word) %>% summarize(wordcnt = sum(wordcnt)) %>% 
  top_n(n = 10, wordcnt) %>% arrange(yrbucket, desc(wordcnt)) %>% slice(1:10) %>% print(n = Inf)

top10cntrywords = top10cntrywords %>% group_by(yrbucket) %>% mutate(x = 1, y = n():1)
ggplot() + geom_text(data = top10cntrywords, aes(x = x, y = y, label = word)) + 
  facet_grid(~yrbucket) + theme_void(20)


tmp = word_doc_cntfilt %>% filter(word == "nuclear") %>% group_by(year) %>% summarize(totcnt = sum(wordcnt))
ggplot() + geom_line(data = tmp, aes(x = year, y = totcnt)) + theme_bw()

tmp = word_doc_cntfilt %>% filter(word == "poverty") %>% group_by(year) %>% summarize(totcnt = sum(wordcnt))
ggplot() + geom_line(data = tmp, aes(x = year, y = totcnt)) + theme_bw()

# detect trending words with PCA

# count of words in each year
wordyrcnt = word_doc_cntfilt %>% group_by(year, word) %>% summarize(wordcnt = sum(wordcnt))
# total words in each year
yrcnt = word_doc_cntfilt %>% group_by(year) %>% summarize(yrwordcnt = sum(wordcnt))

# get percentage of words in each year
wordyrcnt = inner_join(wordyrcnt, yrcnt, by = "year") %>%
  mutate(wordpct = wordcnt / yrwordcnt)

# reshape to the form year x words for input to PCA
wordyrcnt_s = wordyrcnt %>% select(year, word, wordpct) %>% spread(word, wordpct, fill = 0)

# PCA
pcamdl = prcomp(wordyrcnt_s[-1])

# variance explained plot
varexp = cumsum(pcamdl$sdev^2)/sum(pcamdl$sdev^2)
varexpdf = data.frame(PC = seq(1, length(varexp)), varexp = varexp)
ggplot() + geom_line(data = varexpdf, aes(x = PC, y = varexp)) + expand_limits(y = 0) + theme_bw()

varexpdf[1:2,]

scoresdf = data.frame(pcamdl$x)
names(scoresdf) = paste0("PC", seq(1, ncol(pcamdl$x)))
scoresdf$year = wordyrcnt_s$year

loadingsdf = data.frame(pcamdl$rotation)
loadingsdf$word = row.names(loadingsdf)

pickpc = "PC1"

ggplot() + geom_line(data = scoresdf, aes_string(x = "year", y = pickpc)) + theme_bw()

tmploadings = loadingsdf[,c("word", pickpc)]
names(tmploadings) = c("word", "loading")

topwords = tmploadings %>% arrange(desc(loading)) %>% slice(1:5) %>% print()
botwords = tmploadings %>% arrange(loading) %>% slice(1:5) %>% print()

ggplot() + geom_line(data = wordyrcnt[wordyrcnt$word %in% topwords$word,], 
                     aes(x = year, y = wordpct, color = word)) + theme_bw()

ggplot() + geom_line(data = wordyrcnt[wordyrcnt$word %in% botwords$word,], 
                     aes(x = year, y = wordpct, color = word)) + theme_bw()

# topic models
pickyr = 2015
dtm_filtyr = word_doc_cntfilt %>% filter(year == pickyr) %>% 
  cast_dtm(docid, word, wordcnt)
str(dtm_filtyr)
dim(dtm_filtyr)

word_doc_cntyrbucket = word_doc_cntfilt %>% group_by(yrbucket, country, word) %>% 
           summarize(wordcnt = sum(wordcnt))
yrbucket_country_id = word_doc_cntyrbucket %>% group_by(yrbucket, country) %>%
                 summarize(cnt = n()) %>% ungroup() %>% mutate(docid = 1:n())
word_doc_cntyrbucket = inner_join(word_doc_cntyrbucket, 
                                  yrbucket_country_id[,c("docid", "yrbucket", "country")], by = c("yrbucket", "country"))

dtm = word_doc_cntyrbucket %>% cast_dtm(docid, word, wordcnt)
str(dtm)
dim(dtm)


topicmdl = LDA(dtm, k = 50, control = list(seed = 1234, verbose = TRUE))

topicTerms = terms(topicmdl, 10)
topicTerms

topicTopics = topics(topicmdl)
cntrylist = word_doc_cntfilt %>% filter(year == pickyr) %>% group_by(docid, country) %>% slice(1:1)
cntrylist$topic = topicTopics[as.character(cntrylist$docid)]

pickTopic = 6
topicTerms[,pickTopic]
cntrylist %>% filter(topic == pickTopic) %>% select(country) %>% print(n = Inf)

# topic model with R LDA package

# convert data to LDA format
pickyr = 1980
dtm_filtyr = word_doc_cntfilt %>% filter(year == pickyr) %>% 
  cast_dtm(docid, word, wordcnt)
str(dtm_filtyr)
dim(dtm_filtyr)

doclist = list()
for(i in 1:nrow(dtm_filtyr)) {
  doclist[[i]] = matrix(as.integer(c(dtm_filtyr$j[dtm_filtyr$i == i] - 1, 
                                     dtm_filtyr$v[dtm_filtyr$i == i])), byrow = TRUE, nrow = 2
  )
}

vocab = dtm_filtyr$dimnames$Terms

result <- lda.collapsed.gibbs.sampler(doclist,
                                      10,  ## Num clusters
                                      vocab,
                                      100,  ## Num iterations
                                      0.1,
                                      0.1) 

top.words <- top.topic.words(result$topics, 10, by.score=TRUE)


doclist = list()
for(i in 1:nrow(dtm)) {
  doclist[[i]] = matrix(as.integer(c(dtm$j[dtm$i == i] - 1, 
                                     dtm$v[dtm$i == i])), byrow = TRUE, nrow = 2
  )
}

vocab = dtm$dimnames$Terms

result <- lda.collapsed.gibbs.sampler(doclist,
                                      50,  ## Num clusters
                                      vocab,
                                      25,  ## Num iterations
                                      0.1,
                                      0.1) 

top.words <- top.topic.words(result$topics, 10, by.score=TRUE)

