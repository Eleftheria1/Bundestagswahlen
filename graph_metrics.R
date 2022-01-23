# Overall graph metrics
load("doc2vec_graph/politician_level_dco2vec.RData")
load("doc2vec_graph/word_embedding_utils.RData")
library(tidyverse)
library(tidygraph)
library(ggraph)
library(patchwork)

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
                              k = 5)

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
             louvian_communities = group_louvain(weights = weight),
             loc_size1 = local_size(order = 1),
             loc_size2 = local_size(order = 2),
             loc_size3 = local_size(order = 3),
             loc_size5 = local_size(order = 5),
             loc_size8 = local_size(order = 8),
             loc_size100 = local_size(order = 100))
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
    loc_size1 <- filtered_graph %>%
      pull(loc_size1) %>%
      max()
    loc_size2 <- filtered_graph %>%
      pull(loc_size2) %>%
      max()
    loc_size3 <- filtered_graph %>%
      pull(loc_size3) %>%
      max()
    loc_size5 <- filtered_graph %>%
      pull(loc_size5) %>%
      max()
    loc_size8 <- filtered_graph %>%
      pull(loc_size8) %>%
      max()
    loc_size100 <- filtered_graph %>%
      pull(loc_size100) %>%
      max()
    graph_size <- filtered_graph %>%
      pull(name) %>% length()
    c(connectivity = connectivity,
      diameter = diameter * 100,
      mean_dist = mean_dist, 
      n_communities = n_communities,
      loc_size1 = loc_size1,
      loc_size2 = loc_size2,
      loc_size3 = loc_size3,
      loc_size5 = loc_size5,
      loc_size8 = loc_size8,
      loc_size100 = loc_size100,
      graph_size = graph_size)
  })
  plot_metrics <- metrics[1:4, ] %>%
    as_tibble() %>%
    bind_cols(metric = rownames(metrics[1:4, ])) %>%
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
  
  hop_plot <- metrics[5:9, ] %>%
    as_tibble() %>%
    bind_cols(hop = c(1:3, 5, 8)) %>%
    pivot_longer(-hop, names_to = "party") %>%
    group_by(party) %>%
    mutate(value = value / metrics[11, party]) %>%
    ungroup() %>%
    ggplot(aes(x = hop, y = value, col = party)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = c("blue", "black", "darkgrey",
                                  "violet", "orange",
                                  "green", "red"), name = "") +
    scale_x_continuous(breaks = c(1:3, 5, 8)) +
    labs(x = "Number of hops",
         y = "Percentage of coverage of all nodes") +
    theme_light()
  
  hop_plot_communities <- metrics[5:9, ] %>%
    as_tibble() %>%
    bind_cols(hop = c(1:3, 5, 8)) %>%
    pivot_longer(-hop, names_to = "party") %>%
    group_by(party) %>%
    mutate(value = value / metrics[10, party]) %>%
    ungroup() %>%
    ggplot(aes(x = hop, y = value, col = party)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = c("blue", "black", "darkgrey",
                                  "violet", "orange",
                                  "green", "red"), name = "") +
    scale_x_continuous(breaks = c(1:3, 5, 8)) +
    labs(x = "Number of hops",
         y = "Percentage of coverage in the largest community") +
    theme_light()
  
  list(metrics = metrics,
       plot = plot_metrics,
       hop_plot = hop_plot + hop_plot_communities +
         plot_layout(guides = "collect"))
}

# neighboring graph
graph_metrics(neighboring_graph)$plot
graph_metrics(neighboring_graph)$metrics
graph_metrics(neighboring_graph)$hop_plot
# knn graph
graph_metrics(knn_graph)$hop_plot
graph_metrics(knn_graph)$metrics
graph_metrics(knn_graph)$plot

# metrics_neighboring_graph_0.001 <- graph_metrics(neighboring_graph)
# metrics_5nn <- graph_metrics(knn_graph)
# 
# save(metrics_neighboring_graph_0.001, metrics_5nn,
#      file = "doc2vec_graph/graph_metrics.RData")
# 

