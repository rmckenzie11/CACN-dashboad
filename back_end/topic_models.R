library(quanteda)
library(wesanderson)
library(tm)
library(corpus)
library(topicmodels)


df <- readRDS("tidy_comments.rds")
df <- df %>%
  group_by(id) %>%
  summarize(paste(comments, sep = " "))
  unite(col = "texts", comments, sep = " ")

merge_comments <- function(df) {
  pasted <- list()
  for(i in 1:length(df$id)) {
    temp <- df %>%
      filter(id == i)
    pasted[[i]] <- paste(unlist(temp$comments), collapse = " ")
  }
  pasted
}

dat <- merge_comments(df)
dat <- head(dat)
dat <- as.data.frame(do.call(rbind, dat))
dat$id <- 1:6
dat <- dat %>%
  mutate(`V1` = as.character(`V1`))
corp <- corpus(dat, docid_field = "id", text_field = "V1")

freq <- dfm(corp) %>%
  dfm_group(groups = c("1", "2", "3", "4", "5","6")) %>%
  dfm_tfidf() 

freq_df <- as.data.frame(t(freq))

names(freq_df) <- c("word", "doc1", "doc3", "doc4", "doc5", "doc7", "doc8")

meeting_tf <- list()
meeting_tf_plots <- list()
fills <- c(wes_palette("Darjeeling1"), wes_palette("BottleRocket1"))
for(i in 1:6){
  temp = as.character(names(freq_df)[i+1])
  meeting_tf[[i]] <- freq_df %>%
    select(word, temp) 
  tfs <- meeting_tf[[i]][[2]]
  names(tfs) <- meeting_tf[[i]][[1]]
  
  keywords <- tail(sort(tfs),20)
  keywords <- data.frame(word = names(keywords), vals = keywords)
  meeting_tf_plots[[i]] <- ggplot(keywords, aes(x = reorder(word, vals), y = vals)) +
    geom_col(fill = fills[i]) +
    coord_flip()
}

saveRDS(meeting_tf_plots, "tfidf_plots.rds")
