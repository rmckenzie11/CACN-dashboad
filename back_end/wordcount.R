library(tidyverse)

df <- readRDS("tidy_comments.rds")

df <- df %>%
  mutate(fill = case_when(caucus == "Conservative" ~ "blue",
                          caucus == "Liberal" ~ "red",
                          caucus == "NDP" ~ "orange",
                          caucus == "Green Party" ~ "green",
                          caucus == "Witness" ~ "grey",
                          caucus == "Bloc Québécois" ~ "purple")) %>%
  mutate(comments = as.character(comments)) %>%
  unnest_tokens(output = "token", input = comments) %>%
  group_by(id, name, caucus, fill) %>%
  count()

compute_plots <- function(df) {
  outputs <- list()
  for (i in 1:length(table(df$id))) {
    temp <- df %>%
      filter(id == i)
    outputs[[i]] <- ggplot(temp, aes(reorder(name, -n), n, fill = fill)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      coord_flip() +
      scale_fill_identity(guide = "legend", labels = df$caucus, breaks = df$fill) +
      theme_classic() +
      theme(legend.title = element_blank()) +
      xlab("Name") +
      ylab("# of Words")
  }
  outputs[[7]] <- ggplot(df, aes(reorder(name, -n), n, fill = fill)) +
    geom_bar(stat = "identity", width = 0.5, color = "black") +
    coord_flip() +
    scale_fill_identity(guide = "legend", labels = df$caucus, breaks = df$fill) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    xlab("Name") +
    ylab("# of Words")
  outputs
}

all <- compute_plots(df)
mp <- compute_plots(df %>% filter(caucus != "Witness"))

saveRDS(all, "wordcount_plots_all.rds")
saveRDS(mp, "wordcount_mp_all.rds")
