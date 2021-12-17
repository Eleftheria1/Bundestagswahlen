#####################################################
#           Word embedding
#####################################################

btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "character",
                "author_id" = "character",
                "text" = "character",
                "party" = "integer",
                "word_count" = "integer"))


hist(btw17_corpus$word_count)

library(word2vec)
library(tidyverse)
library(tictoc)

# maybe better: tweet level ('sentence') embeddings to reduce the
# influence of not relevant words in the similarity averaging

tic()
skipgram_model <- word2vec(
  x = btw17_corpus$text_stemmed,
  type = "skip-gram",
  dim = 50,
  window = 5,
  sample = 0.00001,
  iter = 200,
  lr = 0.05,
  min_count = 2,
  threads = 2,
  split = c(" ", "\\+")
)
toc()

unique_vocabulary_stemmed <- unique(
  str_split(
    str_c(btw17_corpus$text_stemmed, collapse = " "),
    " "
  )[[1]]
)

word_embedding <- predict(skipgram_model,
                          unique_vocabulary_stemmed,
                          type = "embedding")

# percentage of omitted words due to low occurance
sum(is.na(word_embedding)) / prod(dim(word_embedding))
# visualize the results 

# UWOT
umap_emb <- uwot::umap(
  X = na.omit(word_embedding),
  n_components = 3,
  approx_pow = TRUE,
  n_epochs = 20
)
colnames(umap_emb) <- c("x", "y", "z")

btw_word_counts <- btw17_corpus %>%
  tidytext::unnest_tokens(word, text_stemmed) %>%
  count(id, word, sort = TRUE) %>%
  group_by(word) %>%
  summarise(n = sum(n), .groups = "drop")


# visualize the reduced dimensionality reduction
umap_emb %>%
  as_tibble() %>%
  mutate(word = rownames(umap_emb)) %>%
  left_join(btw_word_counts, by = "word") %>%
  filter(n > 400) %>%
  plotly::plot_ly(x = ~x, y = ~y, z = ~z,
                  marker = list(symbol = "circle",
                                color = "black",
                                size = 2),
                  text = ~word) %>%
  plotly::add_markers(opacity = 0.3) %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      zaxis = list(title = "")
    ),
    title = "Umap dimensionality reduction of word embedding"
  )


# calculate the pairwise similarities for the whole vocabulary
# similarities for words that were not fitted are set to 0.

# too much memory directly implement efficient partywise politician
# similarity matrix
calc_pairwise_word_sim <- function(word_embedding) {
  n_words <- nrow(word_embedding)
  res_matrix <- t(combn(seq(n_words), 2))
  sim_vec <- numeric(nrow(res_matrix))
  for (row_ind in seq(nrow(re_matrix))) {
    sim_vec[row_ind] <- exp(
      -0.5 * sum((word_embedding[res_matrix[row_ind, 1], ] -
                    word_embedding[res_matrix[row_ind, 2], ])^2)
    )
  }
  res_matrix <- cbind(res_matrix, sim_vec)
  res_matrix[is.na(res_matrix)] <- 0
}

pairwise_word_similarity <- calc_pairwise_word_sim(word_embedding)

