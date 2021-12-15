library(tidytext)
library(tidyverse)
# library(tm)
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
  count(id, word, sort = TRUE) %>%
  # --------------------------------------------------------------------------
  # added the left join here
  left_join(btw17_corpus %>%
              select(id, author_id, party),
            by = "id")
  # --------------------------------------------------------------------------

# tfidf weighting grouped by author_id so tfidf weights are constant
# for a certain politician. Maybe better then grouping by party because of
# less restrictions and because of the maybe upcoming graph for each party
# btw_author_level_tfidf <- btw_word_level %>%
#   left_join(btw17_corpus %>%
#               select(id, author_id, party),
#             by = "id") %>%
#   group_by(author_id, word, party) %>%
#   summarise(n = sum(n), .groups = "drop") %>%
#   bind_tf_idf(word, author_id, n)

# --------------------------------------------------------------------------

#' Fit a party specific LDA model with the document level being author_id
#'  (politician)
#'
#' @param word_level tbl with columns id, word, n and! author_id and party
#' @param party_ind index of the party to look at
#' @param stopwords a charchter vector of additional words to not consider
#' @param number_of_topics single value indicating the number of topics 
#' @param weighting_function default tm::weightTf
#' @param beta_gamma logical indicating wheter to extract and return the beta 
#' and gamma tibbles
#'
#' @return list of the fitted_lda_model and optionally the 
#'         tibbles gamma_values and beta_values
fit_party_lda <- function(
  word_level,
  party_ind,
  stopwords,
  number_of_topics,
  weighting_function = tm::weightTf,
  beta_gamma = TRUE
) {
  # create document term matrix
  dtm <- word_level %>%
    filter(party == party_ind) %>%
    filter(!word %in% stopwords) %>%
    count(author_id, word) %>%
    cast_dtm(
      document = author_id,
      term = word,
      weighting = weighting_function,
      value = n
    )
  res_list <- list()
  # fit model
  res_list$fitted_lda_model <- LDA(
    dtm, k = number_of_topics, method = "Gibbs", control = list(seed = 1111)
  )
  if (beta_gamma) {
    res_list$beta_values <- tidy(res_list$fitted_lda_model, matrix = "beta")
    res_list$gamma_values <- tidy(res_list$fitted_lda_model, matrix = "gamma")
  }
  res_list
}

#' Calculates the train test perplexity scores in order to choose a suitable
#' number of topics for LDA topic modeling
#'
#' @param word_level tbl with columns id, word, n and! author_id and party
#' @param train_frac fraction of the data to use for training
#' @param party_ind index of the party to look at
#' @param stopwords a charchter vector of additional words to not consider
#' @param number_of_topics multiple values indicating the number of topics 
#' @param weighting_function default tm::weightTf
#' @param plot logical whether to plot or return the train test perplexity
#'  scores
#'
#' @return the train test perplexity scores either visualized or raw
choose_topic_number <- function(
  word_level,
  train_frac = 0.8,
  party_ind,
  stopwords,
  number_of_topics,
  weighting_function = tm::weightTf,
  plot = TRUE
) {
  # create document term matrix
  dtm <- word_level %>%
    filter(party == party_ind) %>%
    filter(!word %in% stopwords) %>%
    count(author_id, word) %>%
    cast_dtm(
      document = author_id,
      term = word,
      weighting = weighting_function,
      value = n
    )
  train_ind <- sample(nrow(dtm), size = floor(train_frac * nrow(dtm)))
  train <- dtm[train_ind, ]
  test <- dtm[-train_ind, ]
  # one could speed this up with future.apply::sapply() (parallel!)
  perplexities_mat <- sapply(number_of_topics, function(k) {
    model <- LDA(
      train, k = k, method = "Gibbs", control = list(seed = 1111)
    )
    c(
      train_perplexity = perplexity(model, newdata = train),
      test_perplexity = perplexity(model, newdata = test)
    )
  })
  res_tbl <- tibble(
    number_of_topics = number_of_topics,
    train_perplexity = perplexities_mat[1, ],
    test_perplexity = perplexities_mat[2, ]
  )
  if (!plot) {
    return(res_tbl)
  } else {
    res_tbl %>%
      pivot_longer(-number_of_topics) %>%
      mutate(
        name = fct_recode(name,
          train = "train_perplexity",
          test = "test_perplexity"
        ),
        name = fct_inorder(name)
      ) %>%
      ggplot(aes(x = number_of_topics, y = value, col = name)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = number_of_topics) +
      scale_color_manual(values = c("#0c7825", "#5e095a")) +
      labs(x = "Number of topics", y = "Perplexity", col = "") +
      theme_light() +
      theme(panel.grid.minor.x = element_blank())
  }
}

# usage: first choose topic number
# here for afd
choose_topic_number(
  word_level = btw_word_level,
  party_ind = 7,
  train_frac = 0.8,
  stopwords = c("nicht", "afd", "htt", "dwaehl", "af", "kein", "mehr"),
  number_of_topics = c(2:7),
  plot = TRUE
)
#cdu 
choose_topic_number(
  word_level = btw_word_level,
  party_ind = 1,
  train_frac = 0.8,
  stopwords = c("nicht", "cdu", "dwaehl", "kein", "mehr"),
  number_of_topics = c(2:7),
  plot = TRUE
)
#spd
choose_topic_number(
  word_level = btw_word_level,
  party_ind = 2,
  train_frac = 0.8,
  stopwords = c("nicht", "dwaehl", "kein", "mehr"),
  number_of_topics = c(2:7),
  plot = TRUE
)
#linke
choose_topic_number(
  word_level = btw_word_level,
  party_ind = 3,
  train_frac = 0.8,
  stopwords = c("nicht", "dwaehl", "kein", "mehr"),
  number_of_topics = c(2:7),
  plot = TRUE
)
#grüne
choose_topic_number(
  word_level = btw_word_level,
  party_ind = 4,
  train_frac = 0.8,
  stopwords = c("nicht", "dwaehl", "kein", "mehr"),
  number_of_topics = c(2:7),
  plot = TRUE
)
#fdp
choose_topic_number(
  word_level = btw_word_level,
  party_ind = 6,
  train_frac = 0.8,
  stopwords = c("nicht", "dwaehl", "kein", "mehr"),
  number_of_topics = c(2:7),
  plot = TRUE
)
#Summary:
# fdp and afd curve very flat, not a lot of topics -> homogenious?
# cdu, linke semm to be less flat


# then fit the corresponding model
# we have 85 unique politicians and 30230 unique words
# we will use the politicians as document so each politician 
# represents a document and the terms we are looking at are the words
# and the value is the number of appearances of that term in that document
# curve generally here quite flat (=> homogeneity?) so choose minimum i.e. 
# 2 topics.
afd_result <- fit_party_lda(
  word_level = btw_word_level,
  party_ind = 7,
  stopwords = c("nicht", "afd", "htt", "dwaehl", "af"
                , "geht", "gibt", "darum", "kein", "mehr"),
  number_of_topics = 2
)
cdu_result <- fit_party_lda(
  word_level = btw_word_level,
  party_ind = 1,
  stopwords = c("nicht", "dwaehl", "kein", "mehr", "cdu", "heut"
                , "geht", "gibt", "darum"),
  number_of_topics = 2
)
spd_result <- fit_party_lda(
  word_level = btw_word_level,
  party_ind = 2,
  stopwords = c("nicht", "dwaehl", "kein", "mehr", "spd", "heut", "waer",
                "haett", "geht", "gibt", "darum"),
  number_of_topics = 2
)
linke_result <- fit_party_lda(
  word_level = btw_word_level,
  party_ind = 3,
  stopwords = c("nicht", "dwaehl", "kein", "mehr", "link", "heut",
                "geht", "gibt", "darum"),
  number_of_topics = 2
)
gruene_result <- fit_party_lda(
  word_level = btw_word_level,
  party_ind = 4,
  stopwords = c("nicht", "dwaehl", "kein", "mehr", "gruen", "heut",
                "geht", "gibt", "darum", "inn"),
  number_of_topics = 2
)
fdp_result <- fit_party_lda(
  word_level = btw_word_level,
  party_ind = 6,
  stopwords = c("nicht", "dwaehl", "kein", "mehr" , "geht", "gibt", "darum",
                "heut", "ts", "rp", "tl", "cl", "fdp"),
  number_of_topics = 2
)


# dive into the details possibly with more but short utility functions

# e.g. utility function for the nice barplot of top words per topic below
# --------------------------------------------------------------------------
  # Extract the beta and gamma matrices
  # extracting the per-topic-per-word probabilities, called beta, from the model.
  # Notice that this has turned the model into a one-topic-per-term-per-row format. 
  # For each combination, the model computes the probability of that term being 
  # generated from that topic. For example, the term “abend” has a 
  # 0.00130 probability of being generated from topic 1, but a 0.000000588
  # probability of being generated from topic 2.
  # We could use dplyr’s slice_max() to find the 10 terms that are most common 
  # within each topic. 
  top_terms_afd <- afd_result$beta_values %>%
    group_by(topic) %>%
    slice_max(beta, n = 15) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  # we can see that we have basically only two topics not 3
  # one topic where they speak probably a lot about what cdu has done false with
  # migrants etc
  # the other topic where they want to make people vote them and for germany
  top_terms_afd %>%
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
  # if we sort the gammas we can see that author 794628172570619904 = document
  # uses 87% words from topic 1 whereas author 93881135 uses only 12% of
  # the words from topic 1.
  sorted_gammas_afd <- afd_result$gamma_values %>%
    group_by(document) %>%
    arrange(desc(gamma))
  
  # Create grouped gamma tibble
  grouped_gammas_afd <- afd_result$gamma_values %>%
    group_by(document) %>%
    arrange(desc(gamma)) %>%
    slice(1) %>%
    group_by(topic)
  # Count (tally) by topic
  # count the number of words most like each topic.
  # we can see that the most words are in topic 2 (76) the lowest in topic 3
  grouped_gammas_afd %>% 
    tally(topic, sort=TRUE)
  # Average topic weight for top topic for each sentence
  # calculate the average gamma value for each topic
  # we see that the average gamma value for topic 1 is the highest (0.675)
  # so the model estimates that 
  # about 68% of the words in all documents were generated from topic 1.
  grouped_gammas_afd %>% 
    summarise(avg=mean(gamma)) %>%
    arrange(desc(avg))
# 70 in topic 2 50 in topic 1
###############################################################
#cdu
top_terms_cdu <- cdu_result$beta_values %>%
    group_by(topic) %>%
    slice_max(beta, n = 15) %>% 
    ungroup() %>%
    arrange(topic, -beta)
top_terms_cdu %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
sorted_gammas_cdu <- cdu_result$gamma_values %>%
    group_by(document) %>%
    arrange(desc(gamma))
grouped_gammas_cdu <- cdu_result$gamma_values %>%
    group_by(document) %>%
    arrange(desc(gamma)) %>%
    slice(1) %>%
    group_by(topic)
grouped_gammas_cdu %>% 
    tally(topic, sort=TRUE)
grouped_gammas_cdu %>% 
    summarise(avg=mean(gamma)) %>%
    arrange(desc(avg))
# 128 in topic 2 49 in topic 1
#spd
top_terms_spd <- spd_result$beta_values %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms_spd %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
sorted_gammas_spd <- spd_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma))
grouped_gammas_spd <- spd_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)
grouped_gammas_spd %>% 
  tally(topic, sort=TRUE)
grouped_gammas_spd %>% 
  summarise(avg=mean(gamma)) %>%
  arrange(desc(avg))
# 210 in topic 2 74 in topic 1
#linke
top_terms_linke <- linke_result$beta_values %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms_linke %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
sorted_gammas_linke <- linke_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma))
grouped_gammas_linke <- linke_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)
grouped_gammas_linke %>% 
  tally(topic, sort=TRUE)
grouped_gammas_linke %>% 
  summarise(avg=mean(gamma)) %>%
  arrange(desc(avg))
# 102 in topic 2 55 in topic 1
#gruene
top_terms_gruene <- gruene_result$beta_values %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms_gruene %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
sorted_gammas_gruene <- gruene_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma))
grouped_gammas_gruene <- gruene_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)
grouped_gammas_gruene %>% 
  tally(topic, sort=TRUE)
grouped_gammas_gruene %>% 
  summarise(avg=mean(gamma)) %>%
  arrange(desc(avg))
# 222 in topic 2 53 in topic 1
#fdp
top_terms_fdp <- fdp_result$beta_values %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms_fdp %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
sorted_gammas_fdp <- fdp_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma))
grouped_gammas_fdp <- fdp_result$gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)
grouped_gammas_fdp %>% 
  tally(topic, sort=TRUE)
grouped_gammas_fdp %>% 
  summarise(avg=mean(gamma)) %>%
  arrange(desc(avg))
# 87 in topic 1 68 in topic 2 
# ausgeglichen nur bei fdp und afd
# andere parteien hauptsächlich nur ein topic