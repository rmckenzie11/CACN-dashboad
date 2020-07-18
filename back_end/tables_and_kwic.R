library(tidyverse)
library(quanteda)
library(tidytext)

df <- readRDS("tidy_comments.rds")
df <- df %>%
  mutate(comments = as.character(comments)) %>%
  unnest_tokens(output = "token", input = comments)

df$stemmed <- stemmed_tokens
df <- df %>%
  filter(caucus != "Witness")

df2 <- df %>%
  group_by(name, id) %>%
  count(stemmed) %>%
  filter(str_length(stemmed) > 4,
         !stemmed %in% c("committee", "subcommittee", "member", "canada", "china", "minister", "chair", "think", "thank", "agree", "brief", "govern", "issue", "question", "official", "witness", "country", "important", "point", "start", "first", "right"),
         !stemmed %in% stopwords("en")) 

df3 <- df %>%
  group_by(caucus, id) %>%
  count(stemmed) %>%
  filter(str_length(stemmed) > 4,
         !stemmed %in% c("committee", "subcommittee", "member", "canada", "china", "minister", "chair", "think", "thank", "agree", "brief", "govern", "issue", "question", "official", "witness", "country", "important", "point", "start", "first", "right"),
         !stemmed %in% stopwords("en"))

names(df3) <- c("name", "id", "stemmed", "n")
df4 <- rbind(df2, df3)

saveRDS(df4, "wordcount_tables.rds")

df <- readRDS("tidy_comments.rds")
df$comments <- gsub("\\.", "\\. ", df$comments)
names(df) <- c("name", "text_field", "meeting", "caucus", "id")
keywords <- readRDS("dictionary_words.rds")

word <- "health"
keywords <- keywords %>%
  filter(topic == word)

keywords <- keywords$token

temp <- as.data.frame(kwic(df$comments, keywords, window = 15))
temp$meeting <- df$id[as.integer(gsub("[a-z]", "",temp$docname))]
temp$comment <- gsub('.{1}$', '', paste(temp$pre, temp$keyword, temp$post))
temp %>%
  select(meeting, comment) %>%
  distinct()

