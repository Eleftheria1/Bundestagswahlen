### Spectral Clustering + Visualization
load("doc2vec_graph/politician_level_dco2vec.RData")
load("doc2vec_graph/word_embedding_utils.RData")
library(tidyverse)

pairwise_doc_sim <- calc_pairwise_doc_sim(document_embedding)
pairwise2matrix_sim <- function(pairwise_doc_sim, dim) {
  sim_matrix <- matrix(0, nrow = dim, ncol = dim)
  for (i in seq(nrow(pairwise_doc_sim))) {
    sim_matrix[pairwise_doc_sim[i, 1],
               pairwise_doc_sim[i, 2]] <- pairwise_doc_sim[i, 3]
    sim_matrix[pairwise_doc_sim[i, 2],
               pairwise_doc_sim[i, 1]] <- pairwise_doc_sim[i, 3]
  }
  sim_matrix
}
similarity_matrix <- pairwise2matrix_sim(pairwise_doc_sim, nrow(nodes))
degree_vec <- rowSums(similarity_matrix > 0)

calc_spectral_embedding <- function(
  similarity_matrix,
  degree_vec,
  nodes,
  party = NULL
) {
  if (!is.null(party)) {
    filter_ids <- nodes %>%
      filter(group == party) %>%
      pull(id)
  } else {
    filter_ids <- nodes %>%
      pull(id)
  }
  similarity_matrix <- similarity_matrix[filter_ids, filter_ids]
  degree_vec <- degree_vec[filter_ids]
  L <- diag(degree_vec^(-0.5)) %*% similarity_matrix %*% diag(degree_vec^(-0.5))
  eigen_decomp <- eigen(L, symmetric = TRUE)
  elbow_plot <- tibble(x = seq(min(length(eigen_decomp$values), 100)),
                       y = eigen_decomp$values[1:min(length(eigen_decomp$values), 100)]) %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    labs(x = "Eigenvalue", y = "Value") +
    theme_light()
  list(full_embedding = eigen_decomp$vectors,
       elbow_plot = elbow_plot)
}

all_parties_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes
)
all_parties_spec_emb$elbow_plot

afd_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "AFD"
)
afd_spec_emb$elbow_plot

gruene_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "Grüne"
)
gruene_spec_emb$elbow_plot

# clustering the spectral embedding with hierarchical clustering


