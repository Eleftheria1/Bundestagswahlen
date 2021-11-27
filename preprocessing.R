library(tidytext)
library(tidyverse)

btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "character",
                "author_id" = "character",
                "text" = "character",
                "party" = "integer",
                "word_count" = "integer"))

btw17_corpus

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
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
btw_word_level

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

# when using tfidf consider a grouping by party!

# there are a lot of english tweets present they are generally on the
# large side wrt number of words as english stopwords were not
# removed
# detect and remove them? or include in word embedding?
# detect via fasttext package?

# remove tweet with 38 words as it is spam

# general word token dataframe that shows unqiue words with their counts
overall_word_frequency <- btw_word_level %>%
  group_by(word) %>%
  summarize(word_frequency = sum(n)) %>%
  ungroup() %>%
  arrange(desc(word_frequency))

# most frequent words
overall_word_frequency %>%
  head(20)

# overall word frequency distribution
ggplot(overall_word_frequency, aes(x = word_frequency)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Word frequency",
       title = "Distribution of the word frequency") +
  theme_minimal()


# tfidf weighting grouped by party so tfidf weights are constant
# for a certain party
btw_party_level_tfidf <- btw_word_level %>%
  left_join(btw17_corpus %>%
              select(id, party),
            by = "id") %>%
  group_by(party, word) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  bind_tf_idf(word, party, n)

# most important word wrt tfidf
btw_party_level_tfidf %>%
  arrange(desc(tf_idf)) %>%
  head(20)
# most unimportant words wrt tfidf
btw_party_level_tfidf %>%
  arrange(tf_idf) %>%
  head(20)


# scrollable view
btw_party_level_tfidf %>%
  arrange(desc(tf_idf)) %>%
  View()

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
# Calculate the cosine similarity by chapter, using words
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

wordcloud(words = btw_author_level_tfidf$word,
          freq = btw_author_level_tfidf$tf_idf *10,
          max.words=30, random.order=FALSE, rot.per=0.35, 
          scale=c(1,1),
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data= as.data.frame(btw_author_level_tfidf %>%
                                filter(party == 7) %>%
                                select(word, tf_idf) %>%
                                 filter(tf_idf *10 > 1.5)),
           size=0.5, color='random-dark', shape = "circle")
