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
# save(document_embedding, file = "doc2vec_graph/politician_level_dco2vec.RData")
dim(document_embedding)
load("doc2vec_graph/politician_level_dco2vec.RData")

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
                        file = "doc2vec_graph/umap_all_parties.html")

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
  res_matrix
}

pairwise_doc_similarity <- calc_pairwise_doc_sim(document_embedding)
summary(pairwise_doc_similarity[, 3])

sapply(10^-seq(1, 10, 0.5), function(threshold) {
  c(sum(pairwise_doc_similarity[, 3] >= threshold), threshold)
}) %>%
  t() %>%
  as.data.frame() %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_line() +
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "Number of edges", y = "Threshold")

# -> vll threshold 0.00001

######################################################################
#                   Create Neighboring Graph
######################################################################
tweet_counts <- doc_emb_candidates %>%
  left_join(
    btw17_corpus %>%
      group_by(author_id) %>%
      summarize(tweet_count = n()), by = "author_id") %>%
  pull(tweet_count)

nodes <- data.frame(id = 1:nrow(doc_emb_candidates),
                    label = doc_emb_candidates$name,
                    group = doc_emb_candidates$party_fac,
                    value = tweet_counts,
                    title = doc_emb_candidates$name, stringsAsFactors = FALSE)

edges <- data.frame(from = pairwise_doc_similarity[, 1],
                    to = pairwise_doc_similarity[, 2],
                    value = pairwise_doc_similarity[, 3],
                    title = paste0(round(pairwise_doc_similarity[, 3], 5)))

library(tidygraph)
library(ggraph)

graph <- as_tbl_graph(
  data.frame(
    from = pairwise_doc_similarity[, 1],
    to = pairwise_doc_similarity[, 2],
    weight = pairwise_doc_similarity[, 3]
  )
) %>%
  activate(edges) %>%
  filter(weight > 0.017) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    label = doc_emb_candidates$name,
    label_short = str_remove(str_extract(label, ".*,"),","),
    party = doc_emb_candidates$party_fac
  ) %>%
  filter(degree > 0)
graph


ggraph(graph, layout = "stress") +
  geom_edge_link0(aes(edge_width = weight), edge_colour = "grey66",
                  alpha = 0.2) +
  geom_node_point(aes(col = party,size = degree)) +
  geom_node_text(aes(filter = degree >= 25, label = label_short),
                 family = "serif", size = 4, col = "#862ff7") + 
  scale_color_manual(values = c("blue", "black", "darkgrey",
                               "violet", "orange",
                               "green", "red"), name = "") +
  scale_edge_width(range = c(0.01,1)) +
  scale_size(range = c(1.5, 5)) +
  guides(size = "none", edge_width = "none",
         colour = guide_legend(override.aes = list(size = 6))) +
  theme_graph() +
  theme(legend.text = element_text(family = "serif", size = 10))




# works only for small networks
library(visNetwork)
party_graph <- function(party_char, filter_value, color) {
  visNetwork(nodes %>%
               filter(group == party_char),
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
    visGroups(groupname = party_char, color = color) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1))
}

party_graph("CDU", 0.015, "black")
party_graph("FDP", 0.015, "orange")
party_graph("SPD", 0.015, "red")
party_graph("Die Linke", 0.015, "violet")
party_graph("Grüne", 0.015, "darkgreen")
party_graph("AFD", 0.015, "blue")
party_graph("CSU", 0.015, "grey")


######################################################################
#                   Create kNN Graph
######################################################################
create_knn_edges <- function(pairwise_doc_similarity, nodes, k = 5) {
  knn_similarities <- data.frame(from = NULL, to = NULL, sim = NULL)
  for (id in seq(nrow(nodes))) {
    knn_similarities <- knn_similarities %>%
      bind_rows(
        pairwise_doc_similarity %>%
          as.data.frame() %>%
          filter(V1 == id | V2 == id) %>%
          slice_max(order_by = sim_vec, n = k)
      )
  }
  colnames(knn_similarities) <- c("from", "to", "value")
  knn_similarities
}


party_knn_graph <- function(party_char, filter_value = 0, color) {
  visNetwork(nodes %>%
               filter(group == party_char),
             knn_edges %>%
               filter(value >= filter_value) %>%
               filter(from %in% {nodes %>%
                   filter(group == party_char) %>%
                   pull(id)} &
                     to %in% {nodes %>%
                         filter(group == party_char) %>%
                         pull(id)}),
             width = "100%") %>%
    visLegend(useGroups = FALSE) %>%
    visGroups(groupname = party_char, color = color) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1))
}
knn_edges <- create_knn_edges(pairwise_doc_similarity, 3)

party_knn_graph("CDU", color = "black")
party_knn_graph("FDP", color = "orange")
party_knn_graph("AFD", color = "blue")
party_knn_graph("Die Linke", color = "violet")
party_knn_graph("Grüne", color = "darkgreen")
party_knn_graph("CSU", color = "grey")
party_knn_graph("SPD", color = "red")

# static overall knn graph
library(tidygraph)
library(ggraph)

graph_knn <- as_tbl_graph(
  data.frame(
    from = knn_edges$from,
    to = knn_edges$to,
    weight = knn_edges$value
  )
) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    label = doc_emb_candidates$name,
    label_short = str_remove(str_extract(label, ".*,"),","),
    party = doc_emb_candidates$party_fac
  ) %>%
  filter(degree > 0)
graph_knn


ggraph(graph_knn, layout = "graphopt") +
  geom_edge_link0(aes(edge_width = weight), edge_colour = "grey66",
                  alpha = 0.2) +
  geom_node_point(aes(col = party,size = degree)) +
  geom_node_text(aes(filter = degree >= 25, label = label_short),
                 family = "serif", size = 4, col = "#862ff7") + 
  scale_color_manual(values = c("blue", "black", "darkgrey",
                                "violet", "orange",
                                "green", "red"), name = "") +
  scale_edge_width(range = c(0.01,1)) +
  scale_size(range = c(1.5, 5)) +
  guides(size = "none", edge_width = "none",
         colour = guide_legend(override.aes = list(size = 6))) +
  theme_graph() +
  theme(legend.text = element_text(family = "serif", size = 10))

# save(calc_pairwise_doc_sim, nodes, create_knn_edges,
#      file = "doc2vec_graph/word_embedding_utils.RData")
