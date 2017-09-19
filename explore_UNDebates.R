#
#  Explore UN debates dataset
#
#  Dataset downloaded from Kaggle Datasets
#  https://www.kaggle.com/unitednations/un-general-debates
#
#

library(dplyr)
library(cleanNLP)
library(tm)
library(topicmodels)

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

#-------------------------------------------
# Annotate document with Spacy
#-------------------------------------------
init_spaCy()
dfanno = run_annotators(df2$text, as_strings = TRUE)

# save annotation
write_annotation(dfanno, "Data/Annotators_spacy/")

#-----------------Explore text data----------
# load saved annotation
dfanno = read_annotation("Data/Annotators_spacy/")

# meta data for talks
df_meta = df2 %>% select(-text)
df_meta$id = seq(1, nrow(df_meta))

# add year/country info to tokens
df_token = get_token(dfanno)
df_token = inner_join(df_token, df_meta, by = "id")

# pick year
pickYr = 2016

# Most common nouns

topnouns = df_token %>% filter(upos == "NOUN") %>% count(lemma) %>% top_n(n = 20, n) %>% 
  arrange(desc(n)) 
topnouns %>% print(n = Inf)

toppropnouns = df_token %>% filter(upos == "PROPN") %>% count(lemma) %>% top_n(n = 20, n) %>% 
  arrange(desc(n)) 
toppropnouns %>% print(n = Inf)

top_nonpropernouns = df_token %>% filter(pos %in% c("NN", "NNS")) %>% count(lemma) %>% top_n(n = 20, n) %>% 
  arrange(desc(n)) 
top_nonpropernouns %>% print(n = Inf)
  
df_tfidf = df_token %>% filter(pos %in% c("NN", "NNS")) %>% 
           get_tfidf(min_df = 0.05, max_df = 0.95, type = "tfidf", tf_weight = "dnorm")

dim(df_tfidf$tfidf)
head(df_tfidf$vocab, 50)
head(df_tfidf$id, 50)

df_token2 = inner_join(df_token, data.frame(word = df_tfidf$vocab, stringsAsFactors = FALSE), by = "word")

tmp = df_token2 %>% inner_join(df_meta, by = "id") %>%
  count(year, word) %>% group_by(year) %>% 
  top_n(n = 20, n) %>% arrange(year, desc(n))

df_tf = df_token %>% filter(pos %in% c("NN", "NNS")) %>% 
  get_tfidf(min_df = 0.05, max_df = 0.95, type = "tf", tf_weight = "raw")

dim(df_tf$tf)

topicmdl = LDA(df_tf$tf, k = 50, control = list(verbose = 1))

terms = posterior(topicmdl)$terms
topics = posterior(topicmdl)$topics

top_terms <- apply(terms, 1,
                   function(v) paste(df_tf$vocab[order(v,
                                  decreasing = TRUE)[1:5]], collapse = ", "))
