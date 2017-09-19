#
#  Explore UN debates dataset
#
#  Dataset downloaded from Kaggle Datasets
#  https://www.kaggle.com/unitednations/un-general-debates
#
#

library(dplyr)
library(tm)
library(topicmodels)
library(ggplot2)
library(plotly)

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

# Explore data

# Number of countries
length(unique(df2$country))

# count number of countries presented at each session
session_cntrycnt = df2 %>% group_by(year, country) %>% group_by(year) %>% 
                   summarize(cnt = n())
summary(session_cntrycnt)

# text mining
talks = df2$text

doc = VCorpus(VectorSource(talks))
inspect(doc[1:3])
inspect(doc[[1]])

myStopwords = stopwords("english")
myStopwords

doc2 = tm_map(doc,stripWhitespace)
doc2 = tm_map(doc2,content_transformer(tolower))
doc2 = tm_map(doc2,removeWords,myStopwords)
doc2 = tm_map(doc2,removePunctuation)
doc2 = tm_map(doc2,removeNumbers)

inspect(doc2[[1]])

dtm = DocumentTermMatrix(doc2)
dim(dtm)

# find frequent terms
termFreq = colSums(as.matrix(dtm))
termFreqDesc = sort(termFreq,decreasing = TRUE)
head(termFreqDesc,30)

dtm_flag = ifelse(as.matrix(dtm) > 0, 1, 0)
docFreq = colSums(dtm_flag)/nrow(dtm_flag)
head(sort(docFreq, decreasing = TRUE), 30)

idf = log(1 / docFreq) / log(2)
tfidf = termFreq * idf

head(sort(tfidf, decreasing = TRUE), 30)

quantile(tfidf, probs = seq(0, 1, 0.1))

vocab = names(tfidf)[tfidf > 900]
vocab_tfidf = tfidf[tfidf > 900]
head(sort(vocab_tfidf, decreasing = TRUE))

# Filter document term matrix to selected vocabulary
dtm_filt = dtm[, names(tfidf) %in% vocab]
dim(dtm_filt)

pickyr = 1980
dtm_filtyr = dtm_filt[df2$year == pickyr,]
dim(dtm_filtyr)

# do a pca
pcamdl = prcomp(dtm_filtyr)
varexp = cumsum(pcamdl$sdev^2)/sum(pcamdl$sdev^2)
varexp[1:2]
pcascores = pcamdl$x[,c(1:2)]
pcascores_df = data.frame(pcascores)
pcascores_df$country = df2$country[df2$year == pickyr]


p <- plot_ly(pcascores_df, x = ~PC1, y = ~PC2, type = 'scatter', mode = 'markers',
             text = ~country)
p

pcaloadings = data.frame(pcamdl$rotation[,1:2])
pcaloadings$word = row.names(pcaloadings)

pcaloadings %>% mutate(ld = abs(PC1)) %>% arrange(desc(ld)) %>% slice(1:20) %>% select(word)
pcaloadings %>% mutate(ld = abs(PC2)) %>% arrange(desc(ld)) %>% slice(1:20) %>% select(word)

# topic models

topicmdl = LDA(dtm_filtyr, k = 10, control = list(seed = 1234))

topicTerms = terms(topicmdl, 10)
topicTerms

topicTopics = topics(topicmdl)
cntryList = df2$country[df2$year == pickyr]
cntryList[topicTopics == 1]
