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


tic()
skipgram_model <- word2vec(
  x = btw17_corpus$text_stemmed,
  type = "skip-gram",
  dim = 300,
  window = 5,
  sample = 0.00001,
  iter = 200,
  lr = 0.05,
  min_count = 2,
  threads = 2,
  split = c(" ", "\\+")
)
toc()


document_embedding <- doc2vec(
  skipgram_model,
  newdata = btw17_corpus %>%
    select(doc_id = author_id, text = text_stemmed) %>%
    group_by(doc_id) %>%
    summarise(text = str_c(text, sep = " ", collapse = TRUE),
              .groups = "drop")
)
save(document_embedding, file = "umap/politician_level_dco2vec.RData")
dim(document_embedding)


# get the data about the politicians
candidate_data <- readr::read_csv("candidate_data.csv") %>%
  mutate(author_id = as.character(author_id))
doc_emb_candidates <- tibble(author_id = rownames(document_embedding)) %>%
  left_join(candidate_data, by = "author_id") %>%
  mutate(name = str_c(name, firstname, sep = ", ")) %>%
  select(author_id, party, name) %>%
  mutate(party_fac = factor(case_when(
    party == 1 ~ "CDU",
    party == 2 ~ "SPD",
    party == 3 ~ "Die Linke",
    party == 4 ~ "Grüne",
    party == 5 ~ "CSU",
    party == 6 ~ "FDP",
    party == 7 ~ "AFD",
    TRUE ~ "UNKNOWN"
  )),
  party_col = case_when(
    party == 1 ~ "black",
    party == 2 ~ "red",
    party == 3 ~ "violet",
    party == 4 ~ "green",
    party == 5 ~ "grey",
    party == 6 ~ "orange",
    TRUE ~ "blue"
  )
  )
# visualize the results 

# UWOT
umap_doc_emb <- uwot::umap(
  X = na.omit(document_embedding),
  n_components = 3,
  approx_pow = TRUE,
  n_epochs = 20
)
colnames(umap_doc_emb) <- c("x", "y", "z")


# visualize the reduced dimensionality reduction

umap_all_parties <- umap_doc_emb %>%
  as_tibble() %>%
  bind_cols(doc_emb_candidates) %>%
  plotly::plot_ly(x = ~x, y = ~y, z = ~z, color = ~party_fac,
                  colors = c("blue", "black", "darkgrey",
                             "violet", "orange",
                             "green", "red"),
                  marker = list(symbol = "circle",
                                size = 3),
                  text = ~name) %>%
  plotly::add_markers(opacity = 1) %>%
  plotly::layout(
    legend = list(itemsizing = "constant", font = list(size = 15)),
    scene = list(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      zaxis = list(title = "")
    ),
    title = "Umap dimensionality reduction of politician embedding"
  )

umap_all_parties
htmlwidgets::saveWidget(widget = umap_all_parties,
                        file = "umap/umap_all_parties.html")

# calculate the pairwise similarities for all the politicians
# similarities for politicians that were not fitted are set to 0.

calc_pairwise_doc_sim <- function(doc_embedding) {
  n_docs <- nrow(doc_embedding)
  res_matrix <- t(combn(seq(n_docs), 2))
  sim_vec <- numeric(nrow(res_matrix))
  for (row_ind in seq(nrow(res_matrix))) {
    sim_vec[row_ind] <- exp(
      -0.5 * sum((doc_embedding[res_matrix[row_ind, 1], ] -
                    doc_embedding[res_matrix[row_ind, 2], ])^2)
    )
  }
  res_matrix <- cbind(res_matrix, sim_vec)
  res_matrix[is.na(res_matrix)] <- 0
  # normalize similairty scores
  res_matrix[, 3] <- (res_matrix[, 3] - mean(res_matrix[, 3], na.rm = TRUE)) /
    sd(res_matrix[, 3], na.rm = TRUE)
  res_matrix
}

pairwise_doc_similarity <- calc_pairwise_doc_sim(document_embedding)
summary(pairwise_doc_similarity[, 3])


