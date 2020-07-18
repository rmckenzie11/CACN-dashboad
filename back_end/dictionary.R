library(tm)
library(tidyverse)
library(wesanderson)
library(treemapify)
library(tidytext)

stem_hunspell <- function(term) {
  # look up the term in the dictionary
  stems <- hunspell::hunspell_stem(term)[[1]]
  
  if (length(stems) == 0) { # if there are no stems, use the original term
    stem <- term
  } else { # if there are multiple stems, use the last one
    stem <- stems[[length(stems)]]
  }
  
  stem
}


df <- readRDS("tidy_comments.rds")
df <- df %>%
  mutate(comments = as.character(comments)) %>%
  unnest_tokens(output = "token", input = comments)

stemmed_tokens <- vector()
for(i in df$token){
  temp <- stem_hunspell(i)
  stemmed_tokens <- append(stemmed_tokens, temp)
  cat(".")
}
saveRDS(stemmed_tokens, "stemmed_tokens.rds")
stemmed_tokens <- readRDS("stemmed_tokens.rds")

dict <- data.frame(trade = rep(0,11))
dict["trade"] <- c("trade", "commerce", "economy", "invest", "export", "import", "market", "economic", "wto", "pork", "canola")
dict["security"] <- c("taiwan", "huawei", "5g", "security", "military", "militarize", "safety", rep(NA, 4))
dict["health"] <- c("disease", "health", "hospital", "covid", "coronavirus", "virus", "pandemic", "wuhan", "illness", rep(NA, 2))
dict["rights"] <- c("xinjiang", "uighur", "uighurs", "hong", "right", "protest", "legal", "camp", rep(NA, 3))
dict["democracy"] <- c("democracy", "vote", "suffrage", "freedom", "press", "newspaper", "reporter", "journalist", "censor", "censorship", rep(NA,1))
dict["consular"] <- c("kovrig", "spavor", "schellenberg", "celil", "consular", "consul", "consulate", "arrest", "extradite", rep(NA, 2))
dict["relations"] <- c("cooperate", "partner", "partnership", "relationship", "bilateral", rep(NA, 6))


dict <- as.data.frame(apply(dict,2,function(x)gsub('_', ' ',x)))

dict <- dict %>%
  pivot_longer(cols = 1:7, names_to = "topic") %>%
  drop_na() %>%
  filter(topic != "X1") %>%
  mutate(value = as.character(value))

dict <- tolower(((setNames(as.character(dict$value), dict$topic))))

df$stemmed <- stemmed_tokens

temp <- list()
j = 0
  
for (word in df$stemmed) {
    j = 1 + j
    temp[[j]] <- names(dict)[dict == word]
}
  
df <- df %>%
  mutate(topic = temp)

sorted_df <- df %>%
  filter(topic != "character(0)") 

sorted_df_mp <- sorted_df %>%
  filter(caucus != "Witness")

saveRDS(sorted_df, "dictionary_words.rds")

meeting_plot <- function(df){
  plots <- list()
  counter <- c(1:length(unique(df$id)))
  counter <- append(counter, list(1:length(unique(df$id))))
  k= 0
  for (i in counter) {
    k = k + 1
    temp <- df %>%
      filter(id %in% i) %>%
      mutate(fill = case_when(topic == "trade" ~ "#606c38",
                              topic == "health" ~ "#283618",
                              topic == "rights" ~ "#DA9587",
                              topic == "democracy" ~ "#dda15e",
                              topic == "consular" ~ "#bc6c25",
                              topic == "relations" ~ "#5e6472",
                              topic == "security" ~ "black"),
             topic = as.character(topic)) %>%
      group_by(topic, fill) %>%
      count()
    plots[[k]] <- ggplot(temp, aes(x = topic, y = n, fill = fill)) +
      geom_col() +
      theme_classic() +
      scale_fill_identity()
  }
  plots
}

all_plots <- meeting_plot(sorted_df)
mp_plots <- meeting_plot(sorted_df_mp)

saveRDS(all_plots, "all_meeting_topics.rds")
saveRDS(mp_plots, "mp_meeting_topics.rds")

mp_plot <- function(df, filter_column){
  plots <- list()
  j = 0
  for (i in unlist(unique(df[filter_column]))) {
    j = j + 1
    temp <- df[df[filter_column] == i,]
    temp <- temp %>%
      mutate(fill = case_when(topic == "trade" ~ "#606c38",
                              topic == "health" ~ "#283618",
                              topic == "rights" ~ "#DA9587",
                              topic == "democracy" ~ "#dda15e",
                              topic == "consular" ~ "#bc6c25",
                              topic == "relations" ~ "#5e6472",
                              topic == "security" ~ "black"),
             topic = as.character(topic)) %>%
      group_by(topic, fill) %>%
      count()
    
    plots[[j]] <- ggplot(temp, aes(area = n, fill = fill, label = toupper(topic))) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", place = "centre", color = "white") +
      scale_fill_identity() 
  }
  names(plots) <- unlist(unique(df[filter_column]))
  plots
}

treemaps_topics <-  mp_plot(sorted_df_mp, 1) 
caucus_topics <- mp_plot(sorted_df_mp, 3)
treemaps_topics <- append(treemaps_topics, caucus_topics)

meeting_plot <- function(df, filter_column) {
  treemaps_by_meeting <- list()
for (i in 1:length(unique(df$id))) {
  k = 0
  plots <- list()
  for(j in unlist(unique(df[filter_column]))) {
    k = k + 1
    temp <- df[df[filter_column] == j,]   
    temp <- temp %>%
      filter(id == i) %>%
      mutate(fill = case_when(topic == "trade" ~ "#606c38",
                              topic == "health" ~ "#283618",
                              topic == "rights" ~ "#DA9587",
                              topic == "democracy" ~ "#dda15e",
                              topic == "consular" ~ "#bc6c25",
                              topic == "relations" ~ "#5e6472",
                              topic == "security" ~ "black"),
             topic = as.character(topic)) %>%
      group_by(topic, fill) %>%
      count()
    
    plots[[k]] <- ggplot(temp, aes(area = n, fill = fill, label = toupper(topic))) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", place = "centre", color = "white") +
      scale_fill_identity() 
  }
  names(plots) <- unlist(unique(df[filter_column]))
  treemaps_by_meeting[[i]] <- plots
}
  treemaps_by_meeting
}

treemaps_by_meeting <- meeting_plot(sorted_df_mp, 1)
treemaps_by_caucus <- meeting_plot(sorted_df_mp, 3)

for(i in 1:length(treemaps_by_meeting)){
  treemaps_by_meeting[[i]] <- append(treemaps_by_meeting[[i]], treemaps_by_caucus[[i]])
}

treemaps_topics_nest <- list()
treemaps_topics_nest[[1]] <- treemaps_topics
combined_treemaps <- treemaps_by_meeting %>%
  append(treemaps_topics_nest)

saveRDS(combined_treemaps, "topic_treemaps.rds")
