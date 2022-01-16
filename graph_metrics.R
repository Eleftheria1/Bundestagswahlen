# Overall graph metrics
load("doc2vec_graph/politician_level_dco2vec.RData")
load("doc2vec_graph/word_embedding_utils.RData")
library(tidyverse)
library(tidygraph)
library(ggraph)

pairwise_doc_similarity <- calc_pairwise_doc_sim(document_embedding)

# neighboring graph
neighboring_threshold <- 0.001
neighboring_graph <- as_tbl_graph(
  data.frame(
    from = pairwise_doc_similarity[, 1],
    to = pairwise_doc_similarity[, 2],
    weight = pairwise_doc_similarity[, 3]
  ), directed = FALSE
) %>%
  activate(edges) %>%
  filter(weight > neighboring_threshold) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    label = nodes$label,
    label_short = str_remove(str_extract(label, ".*,"),","),
    party = nodes$group
  )
neighboring_graph

# knn graph
knn_edges <- create_knn_edges(pairwise_doc_similarity,
                              nodes = nodes,
                              k = 7)

knn_graph <- as_tbl_graph(
  data.frame(
    from = knn_edges$from,
    to = knn_edges$to,
    weight = knn_edges$value
  ), directed = FALSE
) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    label = nodes$label,
    label_short = str_remove(str_extract(label, ".*,"),","),
    party = nodes$group
  )
knn_graph

### calculate metrics

graph_metrics <- function(graph) {
  metrics <- sapply(levels(nodes$group), function(party_name) {
    filtered_graph <- graph %>%
      activate(nodes) %>%
      filter(party == party_name) %>%
      mutate(diameter = graph_diameter(weights = weight, directed = FALSE) /
               graph_order(),
             mean_dist = graph_mean_dist(directed = FALSE),
             louvian_communities = group_louvain(weights = weight))
    connectivity <- igraph::transitivity(filtered_graph)
    # relative diameter ~ longest path (larger -> more heterogeneity)
    # relative to number of nodes
    diameter <- filtered_graph %>% 
      pull(diameter) %>% head(1)
    # mean distance between nodes
    mean_dist <- filtered_graph %>% 
      pull(mean_dist) %>% head(1)
    n_communities <- filtered_graph %>%
      pull(louvian_communities) %>%
      unique() %>%
      length() 
    c(connectivity = connectivity,
      diameter = diameter * 100,
      mean_dist = mean_dist, 
      n_communities = n_communities)
  })
  plot_metrics <- metrics %>%
    as_tibble() %>%
    bind_cols(metric = rownames(metrics)) %>%
    pivot_longer(-metric, names_to = "party", values_to = "value") %>%
    group_by(metric) %>%
    mutate(min_val = min(value)) %>%
    ungroup() %>%
    ggplot(aes(x = value, y = party, color = party)) +
    geom_point(size = 3) +
    geom_segment(aes(y = party, yend = party,
                     x = min_val,
                     xend = value),
                 size = 1) +
    coord_flip() +
    scale_color_manual(values = c("blue", "black", "darkgrey",
                                  "violet", "orange",
                                  "green", "red"), name = "") +
    facet_wrap(~metric, scales = "free_y", nrow = 2) +
    labs(y = "") +
    theme_light() +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  list(metrics = metrics,
       plot = plot_metrics)
}
# neighboring graph
graph_metrics(neighboring_graph)$plot
graph_metrics(neighboring_graph)$metrics
# knn graph
graph_metrics(knn_graph)$plot
graph_metrics(knn_graph)$metrics







