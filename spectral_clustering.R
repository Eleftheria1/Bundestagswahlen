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
  party = NULL,
  elbow_xmax = 100
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
  elbow_plot <- tibble(
    x = seq(min(length(eigen_decomp$values), elbow_xmax)),
    y = eigen_decomp$values[1:min(length(eigen_decomp$values), elbow_xmax)]) %>%
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
  nodes = nodes,
  elbow_xmax = 50
)
all_parties_spec_emb$elbow_plot

afd_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "AFD",
  elbow_xmax = 10
)
afd_spec_emb$elbow_plot

gruene_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "Grüne",
  elbow_xmax = 20
)
gruene_spec_emb$elbow_plot

cdu_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "CDU",
  elbow_xmax = 20
)
cdu_spec_emb$elbow_plot

csu_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "CSU",
  elbow_xmax = 20
)
csu_spec_emb$elbow_plot

spd_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "SPD",
  elbow_xmax = 20
)
spd_spec_emb$elbow_plot

linke_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "Die Linke",
  elbow_xmax = 20
)
linke_spec_emb$elbow_plot

fdp_spec_emb <- calc_spectral_embedding(
  similarity_matrix = similarity_matrix,
  degree_vec = degree_vec,
  nodes = nodes,
  party = "FDP",
  elbow_xmax = 20
)
fdp_spec_emb$elbow_plot

# clustering the spectral embedding with hierarchical clustering

get_cluster_object <- function(spectral_embedding, n_eigenvectors = 10,
                               rownames = NULL) {
  if (!is.null(rownames)) {
    rownames(spectral_embedding) <- rownames
  }
  flashClust::hclust(
    d = dist(spectral_embedding[, 1:n_eigenvectors])
  )
}

afd_cluster_obj <- get_cluster_object(
  afd_spec_emb$full_embedding,
  n_eigenvectors = 10,
  rownames = nodes$label[nodes$group == "AFD"]
)
ggdendro::ggdendrogram(afd_cluster_obj) +
  theme(axis.text.x = element_text(size = 6))
summary(factor(cutree(afd_cluster_obj, k = 4)))

gruene_cluster_obj <- get_cluster_object(
  gruene_spec_emb$full_embedding,
  n_eigenvectors = 10
)
ggdendro::ggdendrogram(gruene_cluster_obj)

fdp_cluster_obj <- get_cluster_object(
  fdp_spec_emb$full_embedding,
  n_eigenvectors = 10
)
ggdendro::ggdendrogram(fdp_cluster_obj)

cdu_cluster_obj <- get_cluster_object(
  cdu_spec_emb$full_embedding,
  n_eigenvectors = 10
)

csu_cluster_obj <- get_cluster_object(
  csu_spec_emb$full_embedding,
  n_eigenvectors = 10
)

linke_cluster_obj <- get_cluster_object(
  linke_spec_emb$full_embedding,
  n_eigenvectors = 10
)

spd_cluster_obj <- get_cluster_object(
  spd_spec_emb$full_embedding,
  n_eigenvectors = 10
)

umap_spectral_clust <- function(
  spec_embedding,
  n_cluster,
  nodes,
  party = NULL,
  n_eigenvectors_clust = 3,
  n_eigenvectors_umap = ncol(spec_embedding),
  pure_embedding_space = FALSE
) {
  umap_doc_emb <- uwot::umap(
    X = spec_embedding[, 1:n_eigenvectors_umap],
    n_components = 3,
    approx_pow = TRUE,
    n_epochs = 30
  )
  colnames(umap_doc_emb) <- c("x", "y", "z")
  cluster_obj <- get_cluster_object(
    spec_embedding,
    n_eigenvectors = n_eigenvectors_clust
  )
  cluster_assignments <- factor(cutree(cluster_obj, k = n_cluster))
  if (!is.null(party)) nodes <- dplyr::filter(nodes, group == party)
  
  if (pure_embedding_space) {
    umap_doc_emb <- spec_embedding[, 1:3] %>%
      as.data.frame()
    colnames(umap_doc_emb) <- c("x", "y", "z")
  }
  
  umap_doc_emb %>%
    as_tibble() %>%
    bind_cols(nodes) %>%
    bind_cols("cluster" = cluster_assignments) %>%
    plotly::plot_ly(x = ~x, y = ~y, z = ~z, color = ~cluster,
                    colors = rainbow(n_cluster),
                    marker = list(symbol = "circle",
                                  size = 3),
                    text = ~label) %>%
    plotly::add_markers(opacity = 1) %>%
    plotly::layout(
      legend = list(itemsizing = "constant", font = list(size = 15)),
      scene = list(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        zaxis = list(title = "")
      ),
      title = "Umap dimensionality reduction of spectral embedding"
    )
}

umap_spectral_clust(
  spec_embedding = fdp_spec_emb$full_embedding,
  n_cluster = 5,
  nodes = nodes,
  party = "FDP",
  n_eigenvectors_clust = 3,
  n_eigenvectors_umap = 10,
  pure_embedding_space = TRUE
)

umap_spectral_clust(
  spec_embedding = gruene_spec_emb$full_embedding,
  n_cluster = 5,
  nodes = nodes,
  party = "Grüne",
  n_eigenvectors_clust = 3,
  n_eigenvectors_umap = 10,
  pure_embedding_space = TRUE
)

### color the partywise graph with spectral clustering
library(visNetwork)
edges <- data.frame(from = pairwise_doc_sim[, 1],
                    to = pairwise_doc_sim[, 2],
                    value = pairwise_doc_sim[, 3],
                    title = paste0(round(pairwise_doc_sim[, 3], 5)))
knn_edges <- create_knn_edges(
  pairwise_doc_similarity = pairwise_doc_sim,
  nodes = nodes,
  k = 5
)

party_graph_specclust <- function(
  party_char, 
  cluster_obj, n_cluster,
  nodes, edges,
  filter_value = -Inf
  ) {
  cluster_assignments <- factor(cutree(cluster_obj, k = n_cluster))
  visNetwork(nodes %>%
               filter(group == party_char) %>%
               mutate(group = cluster_assignments),
             edges %>%
               filter(value >= filter_value) %>%
               filter(from %in% {nodes %>%
                   filter(group == party_char) %>%
                   pull(id)} &
                     to %in% {nodes %>%
                         filter(group == party_char) %>%
                         pull(id)}),
             width = "100%") %>%
    visLegend(useGroups = FALSE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
               selectedBy = "group")
}

party_graph_specclust(
  party_char = "FDP",
  cluster_obj = fdp_cluster_obj,
  n_cluster = 10,
  nodes = nodes,
  edges = knn_edges,
  filter_value = 0
)
