# document embedding clustering
load("doc2vec_graph/politician_level_dco2vec.RData")
load("doc2vec_graph/word_embedding_utils.RData")
library(tidyverse)


dim(document_embedding)

doc_emb_clustering <- function(
  document_embedding,
  nodes,
  n_cluster,
  party = NULL,
  branch_width = 1.2
  ) {
  if (!is.null(party)) {
    document_embedding <- document_embedding[nodes$group == party, ]
  }
  cluster_obj <- flashClust::hclust(
    d = dist(document_embedding)
  )
  dendro <- as.dendrogram(cluster_obj) %>%
    dendextend::set("branches_k_color", k = n_cluster) %>%
    dendextend::set("labels", rep("", nrow(document_embedding))) %>%
    dendextend::set("branches_lwd", branch_width) %>%
    dendextend::as.ggdend() %>%
    ggplot() + theme(panel.background = element_rect(fill = "grey"))
  cluster_assignments <- factor(cutree(cluster_obj, k = n_cluster))
  list(dendro = dendro, cluster_assignments = cluster_assignments)
}

all_parties_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = 200,
  party = NULL
)
all_parties_clust$dendro

cluster_size <- nodes %>%
  group_by(group) %>%
  summarise(n_cluster = ceiling(n() * 0.3))

afd_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = cluster_size$n_cluster[cluster_size$group == "AFD"],
  party = "AFD",
  branch_width = 2
)
afd_clust$dendro

fdp_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = cluster_size$n_cluster[cluster_size$group == "FDP"],
  party = "FDP",
  branch_width = 2
)
fdp_clust$dendro

cdu_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = cluster_size$n_cluster[cluster_size$group == "CDU"],
  party = "CDU",
  branch_width = 2
)
cdu_clust$dendro

csu_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = cluster_size$n_cluster[cluster_size$group == "CSU"],
  party = "CSU",
  branch_width = 2
)
csu_clust$dendro

linke_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = cluster_size$n_cluster[cluster_size$group == "Die Linke"],
  party = "Die Linke",
  branch_width = 2
)
linke_clust$dendro

gruene_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = cluster_size$n_cluster[cluster_size$group == "Grüne"],
  party = "Grüne",
  branch_width = 2
)
gruene_clust$dendro

spd_clust <- doc_emb_clustering(
  document_embedding = document_embedding,
  nodes = nodes,
  n_cluster = cluster_size$n_cluster[cluster_size$group == "SPD"],
  party = "SPD",
  branch_width = 2
)
spd_clust$dendro

### UMAP document level clustering
umap_cluster_plot <- function(
  document_embedding,
  nodes,
  cluster_assignments, 
  party = NULL,
  clust_prop = 0.1
) {
  if (!is.null(party)) {
    document_embedding <- document_embedding[nodes$group == party, ]
    nodes <- nodes %>%
      filter(group == party)
  }
  umap_doc_emb <- uwot::umap(
    X = document_embedding,
    n_components = 3,
    approx_pow = TRUE,
    n_epochs = 100
  )
  
  # aggregate small clusters 
  cluster_assignments <- fct_lump_prop(cluster_assignments, prop = clust_prop,
                                       other_level = "Others")
  
  colnames(umap_doc_emb) <- c("x", "y", "z")
  umap_doc_emb %>%
    as_tibble() %>%
    bind_cols(cluster = cluster_assignments,
              label = nodes$label) %>%
    plotly::plot_ly(x = ~x, y = ~y, z = ~z, color = ~cluster,
                    colors = c(rainbow(length(levels(cluster_assignments)) - 1),
                               "lightgrey"),
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
      title = "Clustering of politician embedding plot via UMAP"
    )
}

umap_cluster_plot(
  document_embedding = document_embedding,
  nodes = nodes,
  cluster_assignments = afd_clust$cluster_assignments,
  party = "AFD"
)

umap_cluster_plot(
  document_embedding = document_embedding,
  nodes = nodes,
  cluster_assignments = cdu_clust$cluster_assignments,
  party = "CDU"
)

umap_cluster_plot(
  document_embedding = document_embedding,
  nodes = nodes,
  cluster_assignments = csu_clust$cluster_assignments,
  party = "CSU"
)

umap_cluster_plot(
  document_embedding = document_embedding,
  nodes = nodes,
  cluster_assignments = linke_clust$cluster_assignments,
  party = "Die Linke"
)

umap_cluster_plot(
  document_embedding = document_embedding,
  nodes = nodes,
  cluster_assignments = fdp_clust$cluster_assignments,
  party = "FDP"
)

umap_cluster_plot(
  document_embedding = document_embedding,
  nodes = nodes,
  cluster_assignments = spd_clust$cluster_assignments,
  party = "SPD"
)

umap_cluster_plot(
  document_embedding = document_embedding,
  nodes = nodes,
  cluster_assignments = gruene_clust$cluster_assignments,
  party = "Grüne"
)

### graph viz document level clustering
library(visNetwork)
pairwise_doc_sim <- calc_pairwise_doc_sim(document_embedding)
edges <- data.frame(from = pairwise_doc_sim[, 1],
                    to = pairwise_doc_sim[, 2],
                    value = pairwise_doc_sim[, 3],
                    title = paste0(round(pairwise_doc_sim[, 3], 5)))
knn_edges <- create_knn_edges(
  pairwise_doc_similarity = pairwise_doc_sim,
  nodes = nodes,
  k = 5
)
party_graph_docclust <- function(
  party_char, 
  cluster_assignments,
  nodes, edges,
  clust_prop = 0.1,
  filter_value = -Inf
) {
  # aggregate small clusters 
  cluster_assignments <- fct_lump_prop(cluster_assignments, prop = clust_prop,
                                       other_level = "Others")
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
# knn graphs
party_graph_docclust(
  party_char = "AFD",
  cluster_assignments = afd_clust$cluster_assignments,
  nodes = nodes,
  edges = knn_edges
)
party_graph_docclust(
  party_char = "CDU",
  cluster_assignments = cdu_clust$cluster_assignments,
  nodes = nodes,
  edges = knn_edges
)
party_graph_docclust(
  party_char = "FDP",
  cluster_assignments = fdp_clust$cluster_assignments,
  nodes = nodes,
  edges = knn_edges
)
party_graph_docclust(
  party_char = "SPD",
  cluster_assignments = spd_clust$cluster_assignments,
  nodes = nodes,
  edges = knn_edges
)
party_graph_docclust(
  party_char = "CSU",
  cluster_assignments = csu_clust$cluster_assignments,
  nodes = nodes,
  edges = knn_edges
)
party_graph_docclust(
  party_char = "Die Linke",
  cluster_assignments = linke_clust$cluster_assignments,
  nodes = nodes,
  edges = knn_edges
)
party_graph_docclust(
  party_char = "Grüne",
  cluster_assignments = gruene_clust$cluster_assignments,
  nodes = nodes,
  edges = knn_edges
)

# neighborhood graphs
party_graph_docclust(
  party_char = "AFD",
  cluster_assignments = afd_clust$cluster_assignments,
  nodes = nodes,
  edges = edges,
  filter_value = 0.01
)
party_graph_docclust(
  party_char = "CDU",
  cluster_assignments = cdu_clust$cluster_assignments,
  nodes = nodes,
  edges = edges,
  filter_value = 0.017
)
party_graph_docclust(
  party_char = "FDP",
  cluster_assignments = fdp_clust$cluster_assignments,
  nodes = nodes,
  edges = edges,
  filter_value = 0.017
)
party_graph_docclust(
  party_char = "SPD",
  cluster_assignments = spd_clust$cluster_assignments,
  nodes = nodes,
  edges = edges,
  filter_value = 0.017
)
party_graph_docclust(
  party_char = "CSU",
  cluster_assignments = csu_clust$cluster_assignments,
  nodes = nodes,
  edges = edges,
  filter_value = 0.017
)
party_graph_docclust(
  party_char = "Die Linke",
  cluster_assignments = linke_clust$cluster_assignments,
  nodes = nodes,
  edges = edges,
  filter_value = 0.017
)
party_graph_docclust(
  party_char = "Grüne",
  cluster_assignments = gruene_clust$cluster_assignments,
  nodes = nodes,
  edges = edges,
  filter_value = 0.017
)
# save(doc_emb_clustering, party_graph_docclust, umap_cluster_plot,
#      file = "doc2vec_graph/doc_clust_utils.RData")
