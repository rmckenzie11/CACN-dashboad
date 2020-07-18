library(tidyverse)

df <- readRDS("dictionary_words.rds")
df <- df %>%
  mutate(topic = unlist(topic)) %>%
  group_by(topic, id) %>%
  count() %>%
  mutate(color = case_when(topic == "trade" ~ "#606c38",
                           topic == "health" ~ "#283618",
                           topic == "rights" ~ "#DA9587",
                           topic == "democracy" ~ "#dda15e",
                           topic == "consular" ~ "#bc6c25",
                           topic == "relations" ~ "#5e6472",
                           topic == "security" ~ "black"))

saveRDS(df, "topics_all_time.rds")

df <- readRDS("dictionary_words.rds")
df <- df %>%
  filter(caucus != "Witness") %>%
  mutate(topic = unlist(topic)) %>%
  group_by(topic, id) %>%
  count() %>%
  mutate(color = case_when(topic == "trade" ~ "#606c38",
                           topic == "health" ~ "#283618",
                           topic == "rights" ~ "#DA9587",
                           topic == "democracy" ~ "#dda15e",
                           topic == "consular" ~ "#bc6c25",
                           topic == "relations" ~ "#5e6472",
                           topic == "security" ~ "black"))

saveRDS(df, "topics_mp_time.rds")


df <- readRDS("dictionary_words.rds")
df <- df %>%
  filter(caucus != "Witness") %>%
  mutate(topic = unlist(topic))
mp_topics <- list()
k = 0
for(i in unique(df$name)){
  k = k + 1
  temp <- df %>%
    filter(name == i) %>%
    group_by(topic, id) %>%
    count() %>%
    mutate(color = case_when(topic == "trade" ~ "#606c38",
                             topic == "health" ~ "#283618",
                             topic == "rights" ~ "#DA9587",
                             topic == "democracy" ~ "#dda15e",
                             topic == "consular" ~ "#bc6c25",
                             topic == "relations" ~ "#5e6472",
                             topic == "security" ~ "black"))
  mp_topics[[k]] <- temp
}

names(mp_topics) <- unique(df$name)

party_topics <- list()
k = 0
for(i in unique(df$caucus)){
  k = k + 1
  temp <- df %>%
  filter(caucus == i) %>%
    mutate(topic = unlist(topic)) %>%
    group_by(topic, id) %>%
    count() %>%
    mutate(color = case_when(topic == "trade" ~ "#606c38",
                             topic == "health" ~ "#283618",
                             topic == "rights" ~ "#DA9587",
                             topic == "democracy" ~ "#dda15e",
                             topic == "consular" ~ "#bc6c25",
                             topic == "relations" ~ "#5e6472",
                             topic == "security" ~ "black"))
  party_topics[[k]] <- temp
}

names(party_topics) <- unique(df$caucus)
combined_topics <- append(mp_topics, party_topics)

saveRDS(combined_topics, "individual_barplots.rds")
