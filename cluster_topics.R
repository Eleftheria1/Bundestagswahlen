# clustering topics
load("doc2vec_graph/politician_level_dco2vec.RData")
load("doc2vec_graph/word_embedding_utils.RData")
load("doc2vec_graph/doc_clust_utils.RData")
library(tidyverse)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
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
  clust_prop = 0.02,
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
  slice_head(n = 20) %>% 
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

clust_tfidf_spd <- cluster_tfidf(
  corpus = btw17_corpus,
  nodes = nodes,
  document_embedding = document_embedding,
  party = "SPD",
  n_cluster = cluster_size$n_cluster[cluster_size$group == "SPD"]
)

clust_tfidf_spd %>%
  group_by(cluster) %>%
  slice_head(n = 15) %>% 
  View()

clust_tfidf_csu <- cluster_tfidf(
  corpus = btw17_corpus,
  nodes = nodes,
  document_embedding = document_embedding,
  party = "CSU",
  n_cluster = cluster_size$n_cluster[cluster_size$group == "CSU"],
  clust_prop = 0.03
)

clust_tfidf_csu %>%
  group_by(cluster) %>%
  slice_head(n = 15) %>% 
  View()
###########################################################################
##########################################################################
                              # wordclouds
##########################################################################
set.seed(222)
#######################################
## AFD 
######################################
#cluster 2
# barcelona 17 august 2017 terroranschlag 
wordcloud2(data= as.data.frame(clust_tfidf_afd %>%
                                 filter(cluster == 2) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 1
wordcloud2(data= as.data.frame(clust_tfidf_afd %>%
                                 filter(cluster == 1) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 6
wordcloud2(data= as.data.frame(clust_tfidf_afd %>%
                                 filter(cluster == 6) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
#######################################
## CDU
######################################
#cluster 2
wordcloud2(data= as.data.frame(clust_tfidf_cdu %>%
                                 filter(cluster == 2) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 8
wordcloud2(data= as.data.frame(clust_tfidf_cdu %>%
                                 filter(cluster == 8) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 6
wordcloud2(data= as.data.frame(clust_tfidf_cdu %>%
                                 filter(cluster == 6) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
#######################################
## SPD
######################################
#cluster 1
wordcloud2(data= as.data.frame(clust_tfidf_spd %>%
                                 filter(cluster == 1) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 3
wordcloud2(data= as.data.frame(clust_tfidf_spd %>%
                                 filter(cluster == 3) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 4
wordcloud2(data= as.data.frame(clust_tfidf_spd %>%
                                 filter(cluster == 4) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
#######################################
## Linke
######################################
#cluster 2
wordcloud2(data= as.data.frame(clust_tfidf_linke %>%
                                 filter(cluster == 2) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 3
wordcloud2(data= as.data.frame(clust_tfidf_linke %>%
                                 filter(cluster == 3) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 6
wordcloud2(data= as.data.frame(clust_tfidf_linke %>%
                                 filter(cluster == 6) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 4
wordcloud2(data= as.data.frame(clust_tfidf_linke %>%
                                 filter(cluster == 4) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
#######################################
## Grüne
######################################
#cluster 1
wordcloud2(data= as.data.frame(clust_tfidf_grüne %>%
                                 filter(cluster == 1) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 3
wordcloud2(data= as.data.frame(clust_tfidf_grüne %>%
                                 filter(cluster == 3) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 2
wordcloud2(data= as.data.frame(clust_tfidf_grüne %>%
                                 filter(cluster == 2) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 8
wordcloud2(data= as.data.frame(clust_tfidf_grüne %>%
                                 filter(cluster == 8) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
#######################################
## FDP
######################################
#cluster 1
wordcloud2(data= as.data.frame(clust_tfidf_fdp %>%
                                 filter(cluster == 1) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
# cluster 3
wordcloud2(data= as.data.frame(clust_tfidf_fdp %>%
                                 filter(cluster == 3) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
#######################################
## CSU
######################################
#cluster 1
wordcloud2(data= as.data.frame(clust_tfidf_csu %>%
                                 filter(cluster == 1) %>%
                                 slice_head(n = 30) %>%
                                 select(word, tf_idf)),
           size=1, color='random-dark', shape = "circle",
           minRotation = -pi/2, maxRotation = -pi/2)
