library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)

btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "character",
                "author_id" = "character",
                "text" = "character",
                "party" = "integer",
                "word_count" = "integer"))

# extract words from each tweet (id) and count the number of word occurances 
# in each tweet. 
btw_word_level <- btw17_corpus %>%
  unnest_tokens(word, text_stemmed) %>%
  count(id, word, sort = TRUE)

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
#Documenttermmatrix
# Create a document term matrix 
  tweet_matrix_afd <- btw_word_level %>%
    left_join(btw17_corpus %>%
                select(id, author_id, party),
              by = "id") %>%
    filter(party == 7) %>% 
    #remove party specific "stopwords"
    filter(!word %in% c("nicht", "afd", "htt", "dwaehl", "af", "kein",
                        "mehr")) %>%
    count(author_id, word) %>%
    # we have 85 unique politicians and 30230 unique words
    # we will use the politicians as document so each politician 
    # represents a document and the terms we are looking at are the words
    # and the value is the number of appearances of that term in that document
    cast_dtm(document = author_id, term = word,
             value = n)#, weighting = tm::weightTfIdf)
  
  # Print the matrix details 
  tweet_matrix_afd

  # Perform Topic Modeling
  sentence_lda <- LDA(tweet_matrix_afd, k = 3, method = 'Gibbs', control = list(seed = 1111))
  # Extract the beta and gamma matrices
  # extracting the per-topic-per-word probabilities, called beta, from the model.
  # Notice that this has turned the model into a one-topic-per-term-per-row format. 
  # For each combination, the model computes the probability of that term being 
  # generated from that topic. For example, the term “abend” has a 
  # 0.00000124 probability of being generated from topic 1, but a 0.000000679
  # probability of being generated from topic 2.
  # We could use dplyr’s slice_max() to find the 10 terms that are most common 
  # within each topic. 
  sentence_betas <- tidy(sentence_lda, matrix = "beta")
  top_terms <- sentence_betas %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  # we can see that we have basically only two topics not 3
  # one topic where they speak probably a lot about what cdu has done false with
  # migrants etc
  # the other topic where they want to make people vote them and for germany
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
  
  
  # Besides estimating each topic as a mixture of words, LDA also models each 
  # document as a mixture of topics. We can examine the per-document-per-topic 
  # probabilities, called gamma
  # Each of these values is an estimated proportion of words from that document 
  # that are generated from that topic. For example, the model estimates that 
  # about 51% of the words in document 1273010575 were generated from topic 1.
  gamma_values <- tidy(sentence_lda, matrix = "gamma")
  # if we sort the gammas we can see that author 794628172570619904 = document
  # uses 87% words from topic 1 whereas author 93881135 uses only 12% of
  # the words from topic 1.
  sorted_gammas <- gamma_values %>%
    group_by(document) %>%
    arrange(desc(gamma))
  
  # Create grouped gamma tibble
  grouped_gammas <- gamma_values %>%
    group_by(document) %>%
    arrange(desc(gamma)) %>%
    slice(1) %>%
    group_by(topic)
  # Count (tally) by topic
  # count the number of words most like each topic.
  # we can see that the most words are in topic 2 (76) the lowest in topic 3
  grouped_gammas %>% 
    tally(topic, sort=TRUE)
  # Average topic weight for top topic for each sentence
  # calculate the average gamma value for each topic
  # we see that the average gamma value for topic 1 is the highest (0.58)
  # so the model estimates that 
  # about 58% of the words in all documents were generated from topic 1.
  grouped_gammas %>% 
    summarise(avg=mean(gamma)) %>%
    arrange(desc(avg))


########################################################
##############################################################
# How to choose the right amount of topics
# maybe we could do also a plot (ellbowcriterion) to choose !!!
# for k = 2 the perplexity scores are training: 3331, test: 3727
# for k = 3 the perplexity scores are training: 3201, test: 3647
# for k = 4 the perplexity scores are training: 3109, test: 3600
# for k = 5 the perplexity scores are training: 3021, test: 3563
# for k = 10 the perplexity scores are training: 2830, test: 3462
# the lower the better
# let's choose k = 4
# Setup train and test data
sample_size <- floor(0.90 * nrow(tweet_matrix_afd))
set.seed(1111)
train_ind <- sample(nrow(tweet_matrix_afd), size = sample_size)
train <- tweet_matrix_afd[train_ind, ]
test <- tweet_matrix_afd[-train_ind, ]

# Peform topic modeling 
lda_model <- LDA(train, k = 10, method = "Gibbs",
                 control = list(seed = 1111))
# Train
perplexity(lda_model, newdata = train) 
# Test
perplexity(lda_model, newdata = test) 
#########################################################


