library(tm)
library(tidytext)
library(Matrix)
library(quanteda)
library(text2vec)
library(tidyverse)
library(scales)
library(data.table)
library(wordcloud)

btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "c",
                "author_id" = "c",
                "text" = "c",
                "party" = "i",
                "word_count" = "i"))

words <- btw17_corpus %>%
  unnest_tokens(output = "word", token = "words", input = text_stemmed) %>%
  count(id, word, sort = TRUE) %>%
  left_join(btw17_corpus %>%
              select(id, author_id, party),
            by = "id")

# All words sorted alphabetically
sort(unique(words$word))
# If you are interested in a specific word but do not know what the stemmed version is,
# you can call this function to search for all words with the first letters of this specific word
# pattern parameter should be a character string to search for
find_word <- function(data = words, pattern) {
  pattern <- paste('^', pattern, sep = "")
  data[grep(pattern, data$word),]
}
# Example: we are interested in the words "covid" and "pandemie", both are not included in  the data 
# because the data are from 2017. To see all matches, call View(find_word(pattern = "interesting_pattern"))
find_word(pattern = "cov")
find_word(pattern = "pand")
# Now when we have found the stemmed version of the interesting word we can see how many times
# it is used per party
# def parameter should be a character string completely matches the stemmed version of the
# interesting word
word_party_dist <- function(data = words, def) {
  sub_data <- data %>%
    filter(word == def) %>%
    arrange(desc(n))
  res <- as.table(tabulate(sub_data$party, nbins = 7))
  names(res) <- c("CDU", "SPD", "Linke", "Gruene", "CSU", "FDP", "AFD")
  print(res)
}
# Some examples
word_party_dist(def = 'nazis')
word_party_dist(def = 'gauland')
word_party_dist(def = 'gruen')
word_party_dist(def = 'klima')

#--------------------------------------------------------------------------------------
# Word embeddings
library(h2o)
h2o.init()

set.seed(123)
# Use 80% of the available data
sample_size <- floor(.8 * nrow(words))
sample_data <- sample(nrow(words), size = sample_size)
# To fit the model save the data as the h2o object
h2o_object = as.h2o(words[sample_data, ])
# Fit the model
word2vec_model <- h2o.word2vec(h2o_object$word)
# Find synonyms for the word of interest, count is the number of synonyms here
h2o.findSynonyms(word2vec_model, "afd", count = 10)
h2o.findSynonyms(word2vec_model, "gruen", count = 10)
h2o.findSynonyms(word2vec_model, "klima", count = 10) # viagra?
# Not really sure how it works and what to do with the fitted word embeddings
# Will investigate it further
#-------------------------------------------------------------------------------------------
# Sentiment analysis (without using word embeddings)
library(qdap)
library(magrittr)
# Predefined functions work well with English, for German we have to load the lexicon
# Lexicon with negative German words
neg_df <- read_tsv("SentiWS_v1.8c_Negative.txt", col_names = FALSE, show_col_types = FALSE)
names(neg_df) <- c("word", "value", "other_forms")

glimpse(neg_df)

neg_df <- neg_df %>% 
  mutate(word = str_sub(word, 1, regexpr("\\|", .$word)-1))
# Lexicon with positive German words
pos_df <- read_tsv("SentiWS_v1.8c_Positive.txt", col_names = FALSE, show_col_types = FALSE)
names(pos_df) <- c("word", "value", "other_forms")
pos_df <- pos_df %>% 
  mutate(word = str_sub(word, 1, regexpr("\\|", .$word)-1))

sentiment_df <- bind_rows("neg" = neg_df, "pos" = pos_df, .id = "neg_pos")

sentiment_df$word <- tolower(sentiment_df$word)
sentiment_df$other_forms <- tolower(sentiment_df$other_forms)

sentiment_df$word <- str_replace_all(
  sentiment_df$word,
  c("\u00c4" = "Ae",
    "\u00e4" = "ae",
    "\u00d6" = "Oe",
    "\u00f6" = "oe",
    "\u00dc" = "Ue",
    "\u00fc" = "ue",
    "\u00df" = "ss"))

sentiment_df$other_forms <- str_replace_all(
  sentiment_df$other_forms,
  c("\u00c4" = "Ae",
    "\u00e4" = "ae",
    "\u00d6" = "Oe",
    "\u00f6" = "oe",
    "\u00dc" = "Ue",
    "\u00fc" = "ue",
    "\u00df" = "ss"))

sentiment_df <- sentiment_df %>% mutate(
  word_stemmed = SnowballC::wordStem(
    word,
    language = "de"))

sentiment_df$word <- NULL
sentiment_df <- rename(sentiment_df, word = word_stemmed)
# The final lexicon
knitr::kable(head(sentiment_df))

#----------------------------------------------------------------------------------------
# Add the sentiment value to each word contained in both: words dataset and lexicon
words %>%
  mutate(linenumber = row_number()) %>%
  inner_join(sentiment_df) %>%
  group_by(linenumber) %>%
  summarise(sentiment = sum(value)) %>%
  left_join(
    words %>%
      mutate(linenumber = row_number())
  ) %>% write.csv("sentiment_output.csv",row.names = FALSE)

sent <- read.csv("sentiment_output.csv")
View(sent)
# I have to fix the id and author_id variables, but linenumber shows us which word each row refers to

unique(words$word) # 102405
sum(unique(words$word) %in% c(sentiment_df$word, sentiment_df$other_forms)) # 2520
# Only 2% of all words are also in the lexicon dataset
# Potentially add some custom words?
#----------------------------------------------------------------------------------------
# Trying different method
neg_df$word <- tolower(neg_df$word)
pos_df$word <- tolower(pos_df$word)

pos_df <- pos_df %>% mutate(
  word = SnowballC::wordStem(
    word,
    language = "de"))

neg_df <- neg_df %>% mutate(
  word = SnowballC::wordStem(
    word,
    language = "de"))

sentiment.lexikon <- dictionary(list(positive = pos_df$word, negative = neg_df$word))
str(sentiment.lexikon)

# Document feature matrix
dfm_sentiment <- dfm(btw17_corpus$text_stemmed, dictionary = sentiment.lexikon)
mds <- convert(dfm_sentiment, to = "data.frame", docid_field = "id")
mds$id <- btw17_corpus$id

# Create a dataset for some graphics later
btw17_corpus_date <- btw17_corpus %>%
  inner_join(candidates_tweets_data, by = "id")
btw17_corpus_date <- btw17_corpus_date %>%
  select(id, text.x, text_stemmed, author_id.x, party, word_count, created_at)

btw17_corpus_date %>% 
  mutate(date = as.Date(str_sub(created_at, 1, regexpr("T", .$created_at)-1)),
         time = str_sub(created_at, start = regexpr("T", .$created_at)+1)) -> btw17_corpus_date

# btw17_corpus_date$month <- format(btw17_corpus_date$date, "%m")

# Graphics for each party showing count of negative and positive words used over time
# Add the title
# CDU
cdu_date <- left_join(btw17_corpus_date, mds, by = "id") %>%
  filter(party == 1) %>%
  gather(positive, negative, key = "polarity", value = "word_count") %>%
  group_by(polarity, date) %>%
  summarize(avg_word_count = mean(word_count))

ggplot(cdu_date, aes(date, avg_word_count, colour = polarity, group = polarity)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1")
#--------------------------------------------------------------------------------------
# SPD
spd_date <- left_join(btw17_corpus_date, mds, by = "id") %>%
  filter(party == 2) %>%
  gather(positive, negative, key = "polarity", value = "word_count") %>%
  group_by(polarity, date) %>%
  summarize(avg_word_count = mean(word_count))

ggplot(spd_date, aes(date, avg_word_count, colour = polarity, group = polarity)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1")
#--------------------------------------------------------------------------------------
# Linke
linke_date <- left_join(btw17_corpus_date, mds, by = "id") %>%
  filter(party == 3) %>%
  gather(positive, negative, key = "polarity", value = "word_count") %>%
  group_by(polarity, date) %>%
  summarize(avg_word_count = mean(word_count))

ggplot(linke_date, aes(date, avg_word_count, colour = polarity, group = polarity)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1")
#--------------------------------------------------------------------------------------
# Gruene
gruene_date <- left_join(btw17_corpus_date, mds, by = "id") %>%
  filter(party == 4) %>%
  gather(positive, negative, key = "polarity", value = "word_count") %>%
  group_by(polarity, date) %>%
  summarize(avg_word_count = mean(word_count))

ggplot(gruene_date, aes(date, avg_word_count, colour = polarity, group = polarity)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1")
#--------------------------------------------------------------------------------------
# CSU
csu_date <- left_join(btw17_corpus_date, mds, by = "id") %>%
  filter(party == 5) %>%
  gather(positive, negative, key = "polarity", value = "word_count") %>%
  group_by(polarity, date) %>%
  summarize(avg_word_count = mean(word_count))

ggplot(csu_date, aes(date, avg_word_count, colour = polarity, group = polarity)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1")
#--------------------------------------------------------------------------------------
# FDP
fdp_date <- left_join(btw17_corpus_date, mds, by = "id") %>%
  filter(party == 6) %>%
  gather(positive, negative, key = "polarity", value = "word_count") %>%
  group_by(polarity, date) %>%
  summarize(avg_word_count = mean(word_count))

ggplot(fdp_date, aes(date, avg_word_count, colour = polarity, group = polarity)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1")
#--------------------------------------------------------------------------------------
# AFD
afd_date <- left_join(btw17_corpus_date, mds, by = "id") %>%
  filter(party == 7) %>%
  gather(positive, negative, key = "polarity", value = "word_count") %>%
  group_by(polarity, date) %>%
  summarize(avg_word_count = mean(word_count))

ggplot(afd_date, aes(date, avg_word_count, colour = polarity, group = polarity)) + 
  geom_line(size = 1) + 
  scale_colour_brewer(palette = "Set1")
#----------------------------------------------------------------------------------------
# How many tweets have how many positive or negative words per party
new_corpus <- left_join(btw17_corpus, mds, by = "id")
# "CDU", "SPD", "Linke", "Gruene", "CSU", "FDP", "AFD"
sentiment_cdu <- new_corpus %>%
  filter(party == 1) %>%
  gather(positive, negative, key = "polarity", value = "word_count")

ggplot(sentiment_cdu, aes(fill = polarity, x = word_count)) + 
  geom_bar(position = "dodge") +
  theme_minimal()
#-------------------------------------------------------------------------------
sentiment_spd <- new_corpus %>%
  filter(party == 2) %>%
  gather(positive, negative, key = "polarity", value = "word_count")

ggplot(sentiment_spd, aes(fill = polarity, x = word_count)) + 
  geom_bar(position = "dodge") +
  theme_minimal()
#--------------------------------------------------------------------------------
sentiment_linke <- new_corpus %>%
  filter(party == 3) %>%
  gather(positive, negative, key = "polarity", value = "word_count")

ggplot(sentiment_linke, aes(fill = polarity, x = word_count)) + 
  geom_bar(position = "dodge") +
  theme_minimal()
#--------------------------------------------------------------------------------
sentiment_gruene <- new_corpus %>%
  filter(party == 4) %>%
  gather(positive, negative, key = "polarity", value = "word_count")

ggplot(sentiment_gruene, aes(fill = polarity, x = word_count)) + 
  geom_bar(position = "dodge") +
  theme_minimal()
#----------------------------------------------------------------------------------
sentiment_csu <- new_corpus %>%
  filter(party == 5) %>%
  gather(positive, negative, key = "polarity", value = "word_count")

ggplot(sentiment_csu, aes(fill = polarity, x = word_count)) + 
  geom_bar(position = "dodge") +
  theme_minimal()
#-----------------------------------------------------------------------------------
sentiment_fdp <- new_corpus %>%
  filter(party == 6) %>%
  gather(positive, negative, key = "polarity", value = "word_count")

ggplot(sentiment_fdp, aes(fill = polarity, x = word_count)) + 
  geom_bar(position = "dodge") +
  theme_minimal()
#---------------------------------------------------------------------------------
sentiment_afd <- new_corpus %>%
  filter(party == 7) %>%
  gather(positive, negative, key = "polarity", value = "word_count")

ggplot(sentiment_afd, aes(fill = polarity, x = word_count)) + 
  geom_bar(position = "dodge") +
  theme_minimal()
#==================================================================================
# One more method for sentiment analysis
library(syuzhet)
lexicon <- sentiment_df %>% select(word, value)
s <- get_sentiment(btw17_corpus$text_stemmed, lexicon = lexicon)
btw17_corpus$sentiment <- s

# The plot has to be adjusted
#plot(btw17_corpus_date$date, s, type = "l", xlab = "Narrative Time",
 #    ylab = "Emotional Valence", col = "red")
#abline(h = 0, col = "black")

# Some statistics per party
# Average polarity
btw17_corpus %>%  group_by(party) %>%
  summarize(mean(sentiment))

# SD of the polarity
btw17_corpus %>% group_by(party) %>%
  summarize(sd(sentiment))

#------------------------------------------------------------------------------------
# Add sentiment values to the corpus with dates
btw17_corpus_date$sentiment <- btw17_corpus$sentiment
#-------------------------------------------------------------------------------------
# Graphics showing the trend of the sentiment of tweets per party per author over time
# Also showing the average sentiment over all authors (who have posted a tweet on that day) 
# for each time point
# CDU
ggplot(subset(btw17_corpus_date, party == 1), aes(date, sentiment, group = author_id.x, col = author_id.x, alpha = 0.5)) + 
  geom_line() + theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), geom = "point", 
                                             fun = mean, shape = 16, size = 1.5)
#----------------------------------------------------------------------------------------------------------------------------
# SPD
ggplot(subset(btw17_corpus_date, party == 2), aes(date, sentiment, group = author_id.x, col = author_id.x, alpha = 0.5)) + 
  geom_line() + theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), geom = "point", 
                                             fun.y = mean, shape = 16, size = 1.5)
#----------------------------------------------------------------------------------------------------------------------------
# Linke
ggplot(subset(btw17_corpus_date, party == 3), aes(date, sentiment, group = author_id.x, col = author_id.x, alpha = 0.5)) + 
  geom_line() + theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), geom = "point", 
                                             fun.y = mean, shape = 16, size = 1.5)
#-----------------------------------------------------------------------------------------------------------------------------
# Gruene
ggplot(subset(btw17_corpus_date, party == 4), aes(date, sentiment, group = author_id.x, col = author_id.x, alpha = 0.5)) + 
  geom_line() + theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), geom = "point", 
                                             fun.y = mean, shape = 16, size = 1.5)
#-----------------------------------------------------------------------------------------------------------------------------
# CSU
ggplot(subset(btw17_corpus_date, party == 5), aes(date, sentiment, group = author_id.x, col = author_id.x, alpha = 0.5)) + 
  geom_line() + theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), geom = "point", 
                                             fun.y = mean, shape = 16, size = 1.5)
#-----------------------------------------------------------------------------------------------------------------------------
# FDP
ggplot(subset(btw17_corpus_date, party == 6), aes(date, sentiment, group = author_id.x, col = author_id.x, alpha = 0.5)) + 
  geom_line() + theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), geom = "point", 
                                             fun.y = mean, shape = 16, size = 1.5)
#-----------------------------------------------------------------------------------------------------------------------------
# AFD
ggplot(subset(btw17_corpus_date, party == 7), aes(date, sentiment, group = author_id.x, col = author_id.x, alpha = 0.5)) + 
  geom_line() + theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), geom = "point", 
                                             fun.y = mean, shape = 16, size = 1.5)
#-----------------------------------------------------------------------------------------------------------------------------

# Frequency analysis
# All words
words_pol <- words %>% 
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )
  
# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#------------------------------------------------------------------------
# CDU
words_pol_cdu <- words %>% 
  filter(party == 1) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol_cdu, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#---------------------------------------------------------------------------
# SPD
words_pol_spd <- words %>% 
  filter(party == 2) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol_spd, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#---------------------------------------------------------------------------
# Linke
words_pol_linke <- words %>% 
  filter(party == 3) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol_linke, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#--------------------------------------------------------------------------
# Gruene
words_pol_gruene <- words %>% 
  filter(party == 4) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol_gruene, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#-------------------------------------------------------------------------
# CSU
words_pol_csu <- words %>% 
  filter(party == 5) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol_csu, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#-----------------------------------------------------------------------
# FDP
words_pol_fdp <- words %>% 
  filter(party == 6) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol_fdp, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#-----------------------------------------------------------------------
# AFD
words_pol_afd <- words %>% 
  filter(party == 7) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, y) %>%
  filter(abs(y) >= 0.7) %>% 
  mutate(
    pos_or_neg = ifelse(y > 0, "positive", "negative")
  )

# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(words_pol_afd, aes(reorder(word, y), y, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))
#--------------------------------------------------------------------------------------------
# word clouds showing most frequent positive and negative words
words_sentiment <- dfm(words$word, dictionary = sentiment.lexikon)
mds_words <- convert(words_sentiment, to = "data.frame", docid_field = "id")
mds_words$id <- words$id

#comparison.cloud(
  #words_sentiment,
  #max.words = 50,
  #colors = c("darkgreen", "darkred")
#)












