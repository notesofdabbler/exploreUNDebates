#
#  Explore UN debates dataset
#
#  Dataset downloaded from Kaggle Datasets
#  https://www.kaggle.com/unitednations/un-general-debates
#
#

#------------load libraries---------------------
library(dplyr)
library(tidyr)
library(tm)
library(topicmodels)
library(ggplot2)
library(ggthemes)
library(tidytext)
library(lda)

#------------ load data -----------------------
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

#---------------Get data in tidy format-------------------------
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

#----------Get word counts and filter words for PCA and topic models ------------
# get word counts for each document
word_doc_cnt = cleaned_df2 %>% group_by(docid, year, country, word) %>% 
                summarize(wordcnt = n())
head(word_doc_cnt)

# calculate tf-idf
totdocs = nrow(df2)
word_freq = word_doc_cnt %>% group_by(word) %>% 
                summarize(totwordcnt = sum(wordcnt), docfreq = n()) %>%
                  mutate(idf = log(totdocs/docfreq), tfidf = totwordcnt * idf, docfreqpct = docfreq/totdocs)

# Check distribution of words in vocabular in terms of % of document they appear in
numwords = table(cut(word_freq$docfreqpct, breaks = seq(0, 1, 0.1)))
numwords

# Check words that appear in more than x% of documents
word_freq %>% filter(docfreqpct >= 0.6) %>% arrange(desc(totwordcnt)) %>% print(n = Inf)
word_freq %>% arrange(desc(tfidf))

# Remove words in vocabulary that are in either less than 5% of talks or in more than 60% of talks
word_freqfilt = word_freq %>% filter(docfreqpct >= 0.05, docfreqpct <= 0.6)
word_doc_cntfilt = inner_join(word_doc_cnt, word_freqfilt[,c("word", "tfidf")], by = "word")

# Create year buckets
yrbucket = cut(word_doc_cntfilt$year, breaks = c(1970, 1980, 1990, 2000, 2010, 2017), 
                 labels = c("70s", "80s", "90s", "2000s", "2010s"), include.lowest = TRUE)
word_doc_cntfilt$yrbucket = yrbucket

# check
word_doc_cntfilt %>% group_by(yrbucket) %>% summarize(minyr = min(year), maxyr = max(year)) %>% print(n = Inf)

#--------------Explore word counts ----------------------
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

#----------detect trending words with PCA------------------------

# Create data for PCA by finding % of words that occur in each year
# PCA data (year x word) with value % of a word that occurs in the year

#---- Create PCA data
# count of words in each year
wordyrcnt = word_doc_cntfilt %>% group_by(year, word) %>% summarize(wordcnt = sum(wordcnt))
# total words in each year
yrcnt = word_doc_cntfilt %>% group_by(year) %>% summarize(yrwordcnt = sum(wordcnt))

# get percentage of words in each year
wordyrcnt = inner_join(wordyrcnt, yrcnt, by = "year") %>%
  mutate(wordpct = wordcnt / yrwordcnt)

# reshape to the form year x words for input to PCA
wordyrcnt_s = wordyrcnt %>% select(year, word, wordpct) %>% spread(word, wordpct, fill = 0)

#---- run PCA
pcamdl = prcomp(wordyrcnt_s[-1])

# variance explained plot
varexp = cumsum(pcamdl$sdev^2)/sum(pcamdl$sdev^2)
varexpdf = data.frame(PC = seq(1, length(varexp)), varexp = varexp)
ggplot() + geom_line(data = varexpdf, aes(x = PC, y = varexp)) + expand_limits(y = 0) + theme_bw()

# variance explained by first 2 PCs
varexpdf[1:2,]

# create scores and loadings dataframe
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

# topic model with R LDA package

# aggregate data to country-year bucket level
word_doc_cntyrbucket = word_doc_cntfilt %>% group_by(yrbucket, country, word) %>% 
           summarize(wordcnt = sum(wordcnt))
yrbucket_country_id = word_doc_cntyrbucket %>% group_by(yrbucket, country) %>%
                 summarize(cnt = n()) %>% ungroup() %>% mutate(docid = 1:n())
word_doc_cntyrbucket = inner_join(word_doc_cntyrbucket, 
                                  yrbucket_country_id[,c("docid", "yrbucket", "country")], by = c("yrbucket", "country"))

# construct document-term matrix
dtm = word_doc_cntyrbucket %>% cast_dtm(docid, word, wordcnt)
str(dtm)
dim(dtm)

# convert document-term matrix to LDA format
doclist = list()
for(i in 1:nrow(dtm)) {
  doclist[[i]] = matrix(as.integer(c(dtm$j[dtm$i == i] - 1, 
                                     dtm$v[dtm$i == i])), byrow = TRUE, nrow = 2
  )
}

vocab = dtm$dimnames$Terms

# run topic model with LDA
result <- lda.collapsed.gibbs.sampler(doclist,
                                      50,  ## Num clusters
                                      vocab,
                                      25,  ## Num iterations
                                      0.1,
                                      0.1) 

# top 5 words in each topic
top.words <- top.topic.words(result$topics, 5, by.score=TRUE)
topiclabel = apply(t(top.words), 1, paste, collapse = " ")

# find topic proportions in each document
topicmap  = t(result$document_sums)
topicmap = topicmap/rowSums(topicmap)
topicmapdf = data.frame(topicmap)
topicmapdf$docid = seq(1, nrow(topicmapdf))

topicmapdfg = topicmapdf %>% gather(topic, topicpct, -docid)
topicmapdfg = inner_join(topicmapdfg, yrbucket_country_id[,c("docid", "yrbucket", "country")], by = "docid")

topicmap_summ = topicmapdfg %>% group_by(yrbucket, topic) %>% summarize(topicpct = mean(topicpct))

ggplot() + geom_bar(data = topicmap_summ, aes(x = yrbucket, y = topicpct), stat = "identity") + 
  facet_wrap(~topic) + theme_bw()

# assign topic with highest proportion to the document
topicmapdfg2 = topicmapdfg %>% group_by(docid) %>% filter(topicpct == max(topicpct))
topicmapdfg2 %>% ungroup() %>% filter(topic == "X13") %>% arrange(desc(topicpct)) %>% 
  distinct(country) %>% slice(1:5) %>% print(n = Inf)

# cluster analysis
pickyrbucket = "90s"
clusdf = inner_join(topicmapdf,
                    yrbucket_country_id[,c("docid", "country", "yrbucket")], by = "docid")
clusdfyr = clusdf %>% filter(yrbucket == pickyrbucket)

clusmdl = kmeans(clusdfyr[,1:50], 10)

clusdfyr$cluster = clusmdl$cluster
clusdfyr = inner_join(clusdfyr, cntryCodes[,c("country", "cntryabb")], by = "country")


p = plot_geo(clusdfyr) %>% add_trace(z = ~cluster, color = ~cluster,
                                text = ~country, locations = ~cntryabb, marker = list(line = l)) %>% 
  layout(title = "hello", geo = g)
p

clusdfyrg = clusdfyr %>%  gather(topic, topicpct, -cluster, -docid, -country, -cntryabb, -yrbucket)
clussumm = clusdfyrg %>% group_by(cluster, topic) %>% summarize(topicpct = mean(topicpct))
clussumm$topicf = factor(clussumm$topic, levels = paste0("X", seq(1, 50)))

ggplot() + geom_bar(data = clussumm, aes(x = topicf, y = topicpct), stat = "identity") + 
      facet_grid(~cluster) + theme_bw() + coord_flip()

pickyrbucket = "2010s"
picktopic = "X47"
tmp = inner_join(topicmapdf[,c(picktopic, "docid")], 
                 yrbucket_country_id[,c("docid", "country", "yrbucket")], by = "docid")
tmp = tmp %>% filter(yrbucket == pickyrbucket)
tmp = inner_join(tmp, cntryCodes[,c("country", "cntryabb")], by = "country")

l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

tmp$topicpct = tmp[[picktopic]]

p = plot_geo(tmp) %>% add_trace(z = ~topicpct, color = ~topicpct, colors = 'Blues',
                                    text = ~country, locations = ~cntryabb, marker = list(line = l)) %>% 
            layout(title = "hello", geo = g)
p
