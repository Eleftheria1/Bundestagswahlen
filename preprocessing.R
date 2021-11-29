library(tidytext)
library(tidyverse)

btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "character",
                "author_id" = "character",
                "text" = "character",
                "party" = "integer",
                "word_count" = "integer"))


# distribution of tweet number per politician
btw17_corpus %>%
  group_by(author_id) %>%
  summarise(n_tweets = n()) %>%
  ggplot(aes(x = n_tweets)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  geom_vline(xintercept = mean(
    btw17_corpus %>%
      group_by(author_id) %>%
      summarise(n_tweets = n()) %>%
      ungroup() %>%
      summarise(median(n_tweets)) %>%
      as.numeric()
  ), col = "blue", size = 1) +
  labs(x = "Number of tweets per politician",
       title = "Distribution of the number of tweets for one politician") +
  theme_minimal()
# the crazy tweeter
btw17_corpus %>%
  group_by(author_id) %>%
  mutate(n_tweets = n()) %>%
  arrange(desc(n_tweets)) %>%
  head(1)

# visual for Distribution of the number of words per tweet
btw17_corpus %>%
  ggplot(aes(x = word_count)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 40, 2)) +
  labs(x = "Words per tweet",
       title = "Distribution of the number of words per tweet") +
  theme_minimal()

btw17_corpus$word_count %>%
  as.factor() %>%
  summary()

# extract words from each tweet (id) and count the number of word occurances 
# in each tweet. Possible problem: as tweets are really short tfidf
# could face problems with no extremely high values
btw_word_level <- btw17_corpus %>%
  unnest_tokens(word, text_stemmed) %>%
  count(id, word, sort = TRUE)

btw_word_level_unstemmed <- btw17_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)

# number of occurances of the same word in the tweets basically
# always 1 -> problematic for tfidf
btw_word_level$n %>%
  as.factor() %>%
  summary()

# have a look at max word occurances
btw_word_level %>%
  group_by(id) %>%
  summarise(max_occurance = max(n)) %>%
  ungroup() %>%
  ggplot(aes(x = max_occurance)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:16) +
  scale_y_log10() +
  labs(x = "Maximal word occurance in tweets",
       y = "log10 counts",
       title = "Distribution of maximal word occurances") +
  theme_minimal()

# when using tfidf consider a grouping by party or politician!

# general word token dataframe that shows unqiue words with their counts
# maybe we will use this later when creating the wordclouds (remove 
# words as "nicht" wich have a very high count but are not so interesting)
overall_word_frequency_unstemmed <- btw_word_level_unstemmed %>%
  group_by(word) %>%
  summarize(word_frequency = sum(n)) %>%
  ungroup() %>%
  arrange(desc(word_frequency))

# most frequent words
overall_word_frequency_unstemmed %>%
  head(20)

# overall word frequency distribution
ggplot(overall_word_frequency_unstemmed, aes(x = word_frequency)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Word frequency",
       title = "Distribution of the word frequency") +
  theme_minimal()


# tfidf weighting grouped by party so tfidf weights are constant
# for a certain party
# btw_party_level_tfidf <- btw_word_level %>%
#   left_join(btw17_corpus %>%
#               select(id, party),
#             by = "id") %>%
#   group_by(party, word) %>%
#   summarise(n = sum(n), .groups = "drop") %>%
#   bind_tf_idf(word, party, n)
# 
# # most important word wrt tfidf
# btw_party_level_tfidf %>%
#   arrange(desc(tf_idf)) %>%
#   head(20)
# # most unimportant words wrt tfidf
# btw_party_level_tfidf %>%
#   arrange(tf_idf) %>%
#   head(20)
# 
# 
# # scrollable view
# btw_party_level_tfidf %>%
#   arrange(desc(tf_idf)) %>%
#   View()

# tfidf weighting grouped by author_id so tfidf weights are constant
# for a certain politician. Maybe better then grouping by party because of
# less restrictions and because of the maybe upcoming graph for each party
btw_author_level_tfidf <- btw_word_level %>%
  left_join(btw17_corpus %>%
              select(id, author_id, party),
            by = "id") %>%
  group_by(author_id, word, party) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  bind_tf_idf(word, author_id, n)


# most important word wrt tfidf
btw_author_level_tfidf %>%
  arrange(desc(tf_idf)) %>%
  head(20)
# most unimportant words wrt tfidf
btw_author_level_tfidf %>%
  arrange(tf_idf) %>%
  head(20)


# scrollable view
btw_author_level_tfidf %>%
  arrange(desc(tf_idf)) %>%
  View()
## ---> no tfidf scores of 0 anymore! good! much better than before

library(widyr)
# Calculate the cosine similarity by politician, using words
comparisons <- btw_author_level_tfidf %>%
  pairwise_similarity(author_id, word, tf_idf) %>%
  arrange(desc(similarity))
# Print the mean of the similarity values
comparisons %>%
  summarize(mean = mean(similarity))

library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)

# for the word clouds remove some very common words without much meaning
# they are kind of "stopwords"
btw_wordcloud_words <- btw_word_level_unstemmed %>% 
  left_join(btw17_corpus %>%
              select(id, author_id, party),
            by = "id") %>%
  filter(!word %in% c("nicht", "heute", "keine", "gut", "mehr","geht",
                      "neu", "jahr", "ja", "immer", "ganz", "waehlt",
                      "eh", "aeh", "elfi", "txl", "na", "ne", "cum",
                      "guter"))

wordcloud(words = btw_wordcloud_words$word,
          freq = btw_wordcloud_words$n,
          max.words=30, random.order=FALSE, rot.per=0.35, 
          scale=c(0.8,0.8),
          colors=brewer.pal(8, "Dark2"))
##########################################
#wordclouds with wordcloud2
# Question what n should we choose ??
# Note: Important! We have different sizes of tweetlengths (or better:
# words used) for each party. so it is kind of obvious that the prob
# for seeing the same word oftener is bigger for parties with more tweets.
# so don't overinterpretate the results regarding heterogenity!
# party size tweets
# cdu 236493, spd 328381, linke 227781, grüne 495825, csu 23175, 
# fdp 166891, afd 331385
# But nevertheless we can see something as the sizes are not so different
# except of grüne wich have a lot more and fdp and csu wich have less
# Note also that it could be that it't alsways the same person who 
# tweets a specific word super often, so keep it in mind when you do
# interpretations regarding heterogenity between politicians! 
set.seed(222)
# wordcloud for afd (60 words) we have to filter for afd because it't 
# too often and you can't see anything except this word, we will do
# this also for the other parties in their wordclouds
wordcloud2(data= as.data.frame(btw_wordcloud_words %>%
                                filter(party == 7) %>%
                                 select(word, n) %>%
                                 group_by(word)%>%
                                 summarize(n = sum(n))%>%
                                 filter(n> 400) %>%
                                 ungroup()) %>%
                                 filter(word != "afd"),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# wordcloud for cdu (30 words)
wordcloud2(data= as.data.frame(btw_wordcloud_words %>%
                                 filter(party == 1) %>%
                                 select(word, n) %>%
                                 group_by(word)%>%
                                 summarize(n = sum(n))%>%
                                 filter(n> 400) %>%
                                 ungroup()%>%
                                 filter(word != "cdu")),
           size=0.7, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# wordcloud for spd (70 words)
wordcloud2(data= as.data.frame(btw_wordcloud_words %>%
                                 filter(party == 2) %>%
                                 select(word, n) %>%
                                 group_by(word)%>%
                                 summarize(n = sum(n))%>%
                                 filter(n> 400) %>%
                                 ungroup()%>%
                                 filter(word != "spd")),
           size=0.5, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# wordcloud for grüne (120 words)

wordcloud2(data= as.data.frame(btw_wordcloud_words %>%
                                 filter(party == 4) %>%
                                 select(word, n) %>%
                                 group_by(word)%>%
                                 summarize(n = sum(n))%>%
                                 filter(n> 400) %>%
                                 ungroup() %>%
                                 filter(word != "gruene")),
           size=0.6, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# wordcloud for fdp (20 words)
wordcloud2(data= as.data.frame(btw_wordcloud_words %>%
                                 filter(party == 6) %>%
                                 select(word, n) %>%
                                 group_by(word)%>%
                                 summarize(n = sum(n))%>%
                                 filter(n> 400) %>%
                                 ungroup() %>%
                                 filter(word != "fdp")),
           size=0.4, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# wordcloud for linke (26 words)
wordcloud2(data= as.data.frame(btw_wordcloud_words %>%
                                 filter(party == 3) %>%
                                 select(word, n) %>%
                                 group_by(word)%>%
                                 summarize(n = sum(n))%>%
                                 filter(n> 400) %>%
                                 ungroup() %>%
                                 filter(word != "linke")),
           size=0.6, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# csu is party == 5 maybe join with cdu ??? 
# not enough counts to plot it with 400 word counts
# we need at most 50 wordcounts to have a meaningfull plot!
wordcloud2(data= as.data.frame(btw_wordcloud_words %>%
                                 filter(party == 5) %>%
                                 select(word, n) %>%
                                 group_by(word)%>%
                                 summarize(n = sum(n))%>%
                                 filter(n> 50) %>%
                                 ungroup() %>%
                                 filter(word != "csu")),
           size=0.8, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# Summary: tbd