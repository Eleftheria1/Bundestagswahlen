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


