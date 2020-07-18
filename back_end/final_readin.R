## Load Libraries

library(readtext)
library(tidyverse)
library(tidytext)
library(stringr)
library(XML)

## First Function: Cleaning raw ourcommons text files

clean_transcript <- function(messy_list) {
  
  clean_df <- list()
  
  for (i in 1:length(messy_list)) {
    temp <- messy_list[[i]]
    temp <- gsub("\\\\n\\\\xa0\\\\n", " skip ", temp)
    temp <- gsub("\\\\xa0", "", temp)
    temp <- gsub("\"", "", temp)
    names <- str_match_all(temp, "skip\\s*(.*?)\\s*:")
    names <- names[[1]][,2]
    names <- names[-length(names)]
    
    comments <- str_match_all(temp, ":\\s*(.*?)\\s*skip")
    comments <- comments[[1]][,2]
    
    meeting_data <- data.frame(names = names, comments = comments)
    meeting_data$names <- gsub("\\(\\s*(.*?)\\s*\\)", "", meeting_data$names)
    meeting_data$names <- gsub(" of the Committee", "", meeting_data$names)
    meeting_data$id <- rep(i, length(meeting_data$names))
    clean_df[[i]] <- meeting_data
  }
  clean_df
}


## Read in CACN raw text data
CACN <- readtext("Data/CACN.txt")$text

## Split data by meeting
CACN <- str_split(CACN, "adjourned")
CACN <- CACN[[1]][-7]

clean_CACN <- clean_transcript(CACN)
clean_CACN = do.call(rbind, clean_CACN)
clean_CACN <- clean_CACN %>%
  separate(names, into = c("honorific", "name"), sep = " ", extra = "merge") %>%
  select(-honorific)
clean_CACN$name <- trimws(clean_CACN$name)

## Load in Members Info
members <- xmlToDataFrame("Data/members.xml") %>%
  select(caucus = CaucusShortName, first = PersonOfficialFirstName, last = PersonOfficialLastName) 
members$caucus <- gsub("Ã©", "é", members$caucus)
members$first <- gsub("Ã©", "é", members$first)
members$last <- gsub("Ã©", "é", members$last)
members$name <- paste(members$first, members$last, sep = " ")
members <- members %>%
  select(caucus, name)

df_caucus <- clean_CACN %>%
  left_join(members)

df_caucus$caucus <- df_caucus$caucus %>%
  replace_na("Witness")

df_caucus$comments <- gsub("\\.", "\\. ", df_caucus$comments)
df_caucus$comments <- gsub("\\\\n", "", df_caucus$comments)
df_caucus$comments <- gsub(", '", ".", df_caucus$comments)
no_periods <- lapply(df_caucus$comments, function(x) {if(str_sub(x, -1) %in% c("?", ".", "-") { paste(x, ".") } else  {x} )})


saveRDS(df_caucus, "tidy_comments.rds")