library(tidyverse)
library(plyr)
library(dplyr)
library(academictwitteR)
library(stringr)
library(stringi)
library(quanteda)
library(data.table)
library(Rcpp)

setwd("C:/Users/Eleftheria/OneDrive/Desktop/fs3/Consulting")
#Read candidate data into RStudio
candidate_data <- read.csv("candidate_data_raw.csv", sep = ";", encoding = "UTF-8")
#Remove candidates without a Twitter account URL from candidate dataset
candidate_data <- drop_na(candidate_data, twlink)

#Extract usernames and store them in separate vector “usernames”
usernames <- c()
for (i in 1:nrow(candidate_data)) {
  username_extract <- str_extract(candidate_data[i, 8], "([^/]+$)")
  usernames <- c(usernames, username_extract)
}

#Import candidates’ tweets data back into RStudio
candidates_tweets_data <- bind_tweet_jsons(data_path = "candidates_tweets_data/")
candidates_users_data <- bind_user_jsons(data_path = "candidates_tweets_data/")

#Extract author IDs from separate list of users
temp <- c()
found = FALSE
for (un in usernames) {
  found = FALSE
  i = 1
  for (user in candidates_users_data$username) {
    if(!is.na(un) & tolower(un) == tolower(user)) {
      found = TRUE
      temp = c(temp, candidates_users_data[i, 11])
      #print(paste(un, user))
      break
    }
    i = i + 1
  }
  if(found == FALSE) {
    temp = c(temp, NA)
    #print(NA)
  }
}
#Check if author has actually posted a tweet
authorIDs <- c()
for(author in temp) {
  check <- is.element(author, unlist(candidates_tweets_data$author_id))
  if(check == TRUE) {
    authorIDs <- c(authorIDs, author)
  }
  else {
    authorIDs <- c(authorIDs, NA)
  }
}
#Add author IDs to candidate dataset and remove rows with missing values
candidate_data$author_id <- authorIDs
candidate_data <- drop_na(candidate_data, author_id)

#Merge candidate dataset and candidates’ tweets dataset, creating the working dataset
working_dataset <- left_join(candidates_tweets_data, candidate_data, by = "author_id")


#working_dataset <- join(candidates_tweets_data, candidate_data,
                        #by = "author_id",
                        #type = "left",
                        #match = "all")
#Save dataset for further use
#################################################
#################################################
# write_rds(working_dataset, "btw17") ??
##################################################
#################################################
#Convert working dataset to data.table object
setDT(working_dataset)

#Replace German umlauts and ligature s'
working_dataset[, text := stringi::stri_trans_general(text, "Any-Latin")]
working_dataset[, text := stringr::str_replace_all(
  text,
  c("\u00c4" = "Ae",
    "\u00e4" = "ae",
    "\u00d6" = "Oe",
    "\u00f6" = "oe",
    "\u00dc" = "Ue",
    "\u00fc" = "ue",
    "\u00df" = "ss"))]

#Remove line breaks, whitespaces, symbols and special characters
working_dataset[, text := stringr::str_replace_all(
  text,
  pattern = "\\n",
  replacement = " ")]

pattern <- stringr::str_c(c(
  "\U0022",
  "\U0027",
  "\U2018",
  "\U2019",
  "\U201C",
  "\U201D",
  "\U201E",
  "\U201F",
  "&amp;",
  "&lt;",
  "&gt;",
  "%",
  " http([^ ]*)",
  "http([^ ]*)",
  "\\\n"),
  collapse = "|")

working_dataset[, text := stringr::str_remove_all(text, pattern)]
working_dataset[, text := stringr::str_squish(text)]

#Extract Twitter-specific symbols (emojis, hashtags, tags)
pattern_emoji <- "[^ -~]"
pattern_hashtag <- "(#)[[:alnum:]]+"
pattern_tag <- "@\\S+"

working_dataset[, text := stringi::stri_unescape_unicode(text)]
working_dataset[, emojis := stringr::str_extract_all(text, pattern_emoji)]
emojis_real <- lapply(
  working_dataset$emojis,
  function(i) unlist(i)[unlist(i) %in% rtweet::emojis$code])
working_dataset$emojis <- emojis_real
working_dataset[, hashtags := stringr::str_extract_all(text, pattern_hashtag)]
working_dataset[, tags := stringr::str_extract_all(text, pattern_tag)]

#Split camel cases back into their original words
pattern_camelcase_hashtag <- "#(.)+[:upper:][:lower:]{2,}"
pattern_split_camelcase <- "(?<=[:lower:])(?=[:upper:])"

working_dataset[, text := lapply(
  .I,
  function(i) {
    components <- unlist(stringr::str_split(text[i], " "))
    case_numbers <- which(stringr::str_detect(
      components, pattern_camelcase_hashtag))
    cases <- components[case_numbers]
    solved_cases <- sapply(
      stringr::str_split(cases, pattern_split_camelcase),
      function(j) paste0(c(j), collapse = " "))
    components[case_numbers] <- solved_cases
    paste0(c(components), collapse = " ")})]

#Remove patterns from text
patterns_to_remove <- stringr::str_c(
  c(pattern_emoji, "#", pattern_tag),
  collapse = "|")

working_dataset[, text := stringr::str_remove_all(text, patterns_to_remove)]
working_dataset[, text := stringr::str_squish(text)]

###############################################################
#         custom stop word/ stemming
#                    approach 
############################################################### 
# reduced dataset for most important info, additional information 
# can be rejoined by the main keys id and author_id
working_dataset_red <- working_dataset %>%
  as.data.frame() %>%
  select(id, text, author_id, party)
remove(working_dataset)

working_dataset_red <- working_dataset_red %>%
  as_tibble() %>%
  mutate(text = tolower(text)) %>%
  # replace _-& with space
  mutate(text = str_replace_all(text, "\\-|\\_|\\&", " ")) %>%
  # only letters allowed
  mutate(text = map_chr(str_extract_all(text, "[:alpha:]+"),
                        ~ str_c(.x, collapse = " "))) %>%
  # remove starting "rt"
  mutate(text = str_remove(text, "^rt")) %>%
  # remove unnecessary white space
  mutate(text = str_trim(str_squish(text))) %>%
  # remove empty and single word tweets
  mutate(word_count = str_count(text, pattern = " ") + 1) %>%
  filter(word_count > 1)

### detect foreign language posts
library(fastText)
language_ident <- language_identification(
  input_obj = working_dataset_red$text,
  pre_trained_language_model_path = "pretrained_languageident.ftz"
)
working_dataset_red <- working_dataset_red %>%
  bind_cols("lang" = language_ident$iso_lang_1) %>%
  filter(lang == "de") %>%
  select(-lang)

### remove stopwords
library(tidytext)
german_stopwords <- get_stopwords("de")
# again change umlaute and use kein and nicht
german_stopwords <- german_stopwords %>%
  mutate(word = str_replace_all(
    word,
    c("\u00e4" = "ae",
      "\u00f6" = "oe",
      "\u00fc" = "ue",
      "\u00df" = "ss"))) %>%
  filter(!str_detect(word, "kein(.)*|nicht"))

# possibly detect even more stopwords
working_dataset_red %>%
  unnest_tokens(word, text) %>%
  anti_join(german_stopwords, by = "word") %>%
  group_by(word) %>%
  dplyr::summarize(count = n()) %>%
  arrange(desc(count)) %>%
  View()

# actually remove the stopwords and apply stemming
working_dataset_red <- working_dataset_red %>%
  unnest_tokens(word, text) %>%
  anti_join(german_stopwords, by = "word") %>%
  # remove detected romaval words from first 100 biggest counts
  filter(!word %in% c("btw",
                      "schon",
                      "mal",
                      "via",
                      "beim",
                      "wer",
                      "unsere",
                      "ab",
                      "ht",
                      "ltwnrw",
                      "wurde",
                      "bundestagswahlen")) %>%
  # remove single letters
  filter(str_length(word) >= 2) %>%
  # add word stemming
  mutate(
    word_stemmed = SnowballC::wordStem(
      word,
      language = "de"
    )
  )

working_dataset_red <- working_dataset_red %>%
  # back to tweet level
  group_by(id) %>%
  dplyr::summarise(text = str_c(word, collapse = " "),
                   text_stemmed = str_c(word_stemmed,
                                        collapse = " "),
                   author_id = dplyr::first(author_id),
                   party = dplyr::first(party),
                   word_count = dplyr::first(word_count))

# save the results
### write to csv
unname(object.size(working_dataset_red) / 1000000) # MB
write_csv(working_dataset_red, "btw17_corpus.csv")  
 ### read in
btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "character",
                "author_id" = "character",
                "text" = "character",
                "text_stemmed" = "character",
                "party" = "integer",
                "word_count" = "integer"))


###############################################################

#Create corpus object containing tweets identified by unique ID
working_dataset[, doc_id := paste(
  id,
  name,
  firstname,
  author_id,
  sep = ""),
  by = seq_len(nrow(working_dataset))]

corpus <- quanteda::corpus(
  working_dataset,
  docid_field = "doc_id",
  text_field = "text")

#Convert tweets into tokens
tokens_corpus <- quanteda::tokens(
  corpus,
  what = "word",
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  remove_punct = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE)

#Lowercase and stem tokens and remove stopwords
tokens_corpus <- quanteda::tokens_tolower(tokens_corpus)
tokens_corpus <- quanteda::tokens_wordstem(
  tokens_corpus,
  language = "german")

clean_and_stem <- function(text) {
  text <-  stringi::stri_trans_general(text, "Any-Latin")
  text <- stringr::str_replace_all(
    text,
    c("\u00c4" = "Ae",
      "\u00e4" = "ae",
      "\u00d6" = "Oe",
      "\u00f6" = "oe",
      "\u00dc" = "Ue",
      "\u00fc" = "ue",
      "\u00df" = "ss"))
  text <- SnowballC::wordStem(text)
  text
}

stopwords <- clean_and_stem(quanteda::stopwords(language = "de"))
stopwords <- unique(stopwords)
stopwords <- stopwords[nchar(stopwords) > 0]
stopwords <- stringr::str_remove_all(
  stopwords,
  "kein(.)*|nicht")
tokens_corpus <- quanteda::tokens_remove(tokens_corpus, stopwords)

tokens_corpus <- quanteda::tokens_select(tokens_corpus, min_nchar = 3)
tokens_corpus <- quanteda::tokens_remove(
  tokens_corpus,
  c("der", "die", "das", "was", "wer", "wie", "ich", "sie", "wir", "ihr", "rt"))

tweet_corpus_tbl <- tibble(raw_name = names(tokens_corpus)) %>%
  mutate(id = str_extract(raw_name, "^\\d+"),
         author_id = str_extract(raw_name, "\\d+$")) %>%
  select(-raw_name) %>%
  left_join(candidate_data %>%
              select(author_id, party),
            by = "author_id") %>%
  rowid_to_column("row_num") %>%
  mutate(text = map_chr(row_num,
                        ~ str_c(tokens_corpus[[.x]],
                                collapse = " "))
           ) %>%
  select(-row_num) %>%
  mutate(word_count = str_count(text, pattern = " ") + 1) %>%
  filter(word_count > 1)

tweet_corpus_tbl

object.size(tweet_corpus_tbl) / 1000000

### write to csv
# write_csv(tweet_corpus_tbl, "btw17_corpus")  
### read in
btw17_corpus <- readr::read_csv(
  "btw17_corpus.csv",
  col_types = c("id" = "character",
                "author_id" = "character",
                "text" = "character",
                "party" = "integer",
                "word_count" = "integer"))






