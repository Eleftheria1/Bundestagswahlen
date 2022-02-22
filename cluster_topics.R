# clustering topics
load("doc2vec_graph/politician_level_dco2vec.RData")
load("doc2vec_graph/word_embedding_utils.RData")
load("doc2vec_graph/doc_clust_utils.RData")
library(tidyverse)
library(tidytext)
btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "character",
                "author_id" = "character",
                "text" = "character",
                "party" = "integer",
                "word_count" = "integer"))

cluster_size <- nodes %>%
  group_by(group) %>%
  summarise(n_cluster = ceiling(n() * 0.3))


cluster_tfidf <- function(
  corpus,
  nodes,
  document_embedding,
  party,
  n_cluster,
  clust_prop = 0.1,
  branch_width = 2
) {
  doc_clust <- doc_emb_clustering(
    document_embedding = document_embedding,
    nodes = nodes,
    n_cluster = n_cluster,
    party = party,
    branch_width = branch_width
  )
  
  cluster_assignments <- fct_lump_prop(
    doc_clust$cluster_assignments,
    prop = clust_prop,
    other_level = "Others")
  
  corpus <- corpus %>%
    left_join(
      tibble(
        author_id = names(cluster_assignments),
        cluster = cluster_assignments),
      by = "author_id"
    ) %>%
    filter(!is.na(cluster))
  corpus_word_level <- corpus %>%
    unnest_tokens(word, text) %>%
    count(id, word, sort = TRUE)
  
  tfidf <- corpus_word_level %>%
    left_join(corpus %>%
                select(id, cluster),
              by = "id") %>%
    group_by(cluster, word) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    bind_tf_idf(word, cluster, n) %>%
    arrange(desc(tf_idf))
  tfidf
}

clust_tfidf_afd <- cluster_tfidf(
  corpus = btw17_corpus,
  nodes = nodes,
  document_embedding = document_embedding,
  party = "AFD",
  n_cluster = cluster_size$n_cluster[cluster_size$group == "AFD"]
)

clust_tfidf_afd %>%
  group_by(cluster) %>%
  slice_head(n = 5) %>% 
  View()

clust_tfidf_cdu <- cluster_tfidf(
  corpus = btw17_corpus,
  nodes = nodes,
  document_embedding = document_embedding,
  party = "CDU",
  n_cluster = cluster_size$n_cluster[cluster_size$group == "CDU"]
)

clust_tfidf_cdu %>%
  group_by(cluster) %>%
  slice_head(n = 10) %>% 
  View()

clust_tfidf_gruene <- cluster_tfidf(
  corpus = btw17_corpus,
  nodes = nodes,
  document_embedding = document_embedding,
  party = "Grüne",
  n_cluster = cluster_size$n_cluster[cluster_size$group == "Grüne"]
)

clust_tfidf_gruene %>%
  group_by(cluster) %>%
  slice_head(n = 10) %>% 
  View()

clust_tfidf_linke <- cluster_tfidf(
  corpus = btw17_corpus,
  nodes = nodes,
  document_embedding = document_embedding,
  party = "Die Linke",
  n_cluster = cluster_size$n_cluster[cluster_size$group == "Die Linke"]
)

clust_tfidf_linke %>%
  group_by(cluster) %>%
  slice_head(n = 15) %>% 
  View()

clust_tfidf_fdp <- cluster_tfidf(
  corpus = btw17_corpus,
  nodes = nodes,
  document_embedding = document_embedding,
  party = "FDP",
  n_cluster = cluster_size$n_cluster[cluster_size$group == "FDP"]
)

clust_tfidf_fdp %>%
  group_by(cluster) %>%
  slice_head(n = 15) %>% 
  View()
