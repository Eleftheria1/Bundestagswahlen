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

skipgram_model <- word2vec(
  x = btw17_corpus$text_stemmed,
  type = "skip-gram",
  dim = 10,
  window = 5,
  iter = 5,
  lr = 0.05,
  min_count = 3,
  threads = 2,
  split = c(" ", "\\+")
)

unique_vocabulary_stemmed <- unique(
  str_split(
    str_c(btw17_corpus$text_stemmed, collapse = " "),
    " "
  )[[1]]
)

word_embedding <- predict(skipgram_model,
                          unique_vocabulary_stemmed,
                          type = "embedding")

# visualize the results 

# UWOT

umap_conf <- umap::umap.defaults
umap_conf$n_components <- 3
umap_conf$n_epochs <- 20
umap_emb <- umap::umap(
  d = word_embedding,
  config = umap_conf
)

umap_emb %>%
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





