library(shiny)
library(DT)
library(quanteda)
library(treemapify)
library(qdapRegex)
library(tidyverse)


## Load in Wordcount Plots
wordcount_all <- readRDS("data/wordcount_plots_all.rds")
wordcount_mp <- readRDS("data/wordcount_mp_all.rds")

## Load in Meeting Topic Barplots
all_meeting_topics <- readRDS("data/all_meeting_topics.rds")
mp_meeting_topics <- readRDS("data/mp_meeting_topics.rds")

## Load in topic modelling
tf <- readRDS("data/tfidf_plots.rds")

## Load in topic treemaps
mp_treemaps <- readRDS("data/topic_treemaps.rds")

## Load in mp wordcount tables
mp_tables <- readRDS("data/wordcount_tables.rds")

## Load in raw text data for KWIC
kwic_text <- readRDS("data/tidy_comments.rds")

## Load in topic line data
topic_line_all <- readRDS("data/topics_all_time.rds")
topic_line_mp <- readRDS("data/topics_mp_time.rds")
individual_barplots <- readRDS("data/individual_barplots.rds")

## Load in dictionary words
dict <- readRDS("data/dictionary_words.rds")

# Define server logic
shinyServer(function(input, output) {
    
    meeting <- reactive({
        meet_check <- input$meet_select
        meet_check <- case_when(meet_check == "All Meetings" ~ 7,
                          meet_check == "Jan 20: Election of Chair" ~ 1,
                          meet_check == "Jan 30: Briefing on bilateral relations and trade" ~ 2,
                          meet_check == "Feb 4: Briefing on consular issues and extradition" ~ 3,
                          meet_check == "Feb 5: Appearance by the Ambassador to China" ~ 4,
                          meet_check == "Feb 24: Briefing from Academics" ~ 5,
                          meet_check == "Mar 9: Committee Business, appearance by former ambassadors to China" ~ 6)
        meet_check
    })
    
    mp <- reactive({
        mp_check <- input$mp_select 
        mp_check <- gsub("All ", "", mp_check)
        mp_check <- gsub(" MPs", "", mp_check)
        mp_check
    })
    
    meeting2 <- reactive({
        meet_check <- input$meet_select2
        meet_check <- case_when(meet_check == "All Meetings" ~ 7,
                                meet_check == "Jan 20: Election of Chair" ~ 1,
                                meet_check == "Jan 30: Briefing on bilateral relations and trade" ~ 2,
                                meet_check == "Feb 4: Briefing on consular issues and extradition" ~ 3,
                                meet_check == "Feb 5: Appearance by the Ambassador to China" ~ 4,
                                meet_check == "Feb 24: Briefing from Academics" ~ 5,
                                meet_check == "Mar 9: Committee Business, appearance by former ambassadors to China" ~ 6)
        meet_check
        })
    
    mp2 <- reactive({
        mp_check <- input$mp_select2 
        mp_check <- gsub("All ", "", mp_check)
        mp_check <- gsub(" MPs", "", mp_check)
        mp_check
        
    })
    
    topic <- reactive({
        input$topic_select
    })
    
    meet_filter_kwic <- reactive({
        filter_check <- input$meet_filter_kwic
        filter_check <- case_when(filter_check == "All Meetings" ~ c(1,3,4,5,7,8),
                                  filter_check == "Jan 20: Election of Chair" ~ 1,
                                  filter_check == "Jan 30: Briefing on bilateral relations and trade" ~ 3,
                                  filter_check == "Feb 4: Briefing on consular issues and extradition" ~ 4,
                                  filter_check == "Feb 5: Appearance by the Ambassador to China" ~ 5,
                                  filter_check == "Feb 24: Briefing from Academics" ~ 7,
                                  filter_check == "Mar 9: Committee Business, appearance by former ambassadors to China" ~ 8)
        filter_check
    })
    
    
    output$wordcount_master <- renderPlot({
        meet_check <- meeting()
        wordcount_all[meet_check]
    })
    
    output$wordcount_mp <- renderPlot({
        meet_check <- meeting()
        wordcount_mp[meet_check]
    })
    
    output$topics_master <- renderPlot({
        meet_check <- meeting()
        all_meeting_topics[meet_check]
    })
    
    output$topics_mp <- renderPlot({
        meet_check <- meeting()
        mp_meeting_topics[meet_check]
    })
    
    output$tf_plot <- renderPlot({
        meet_check <- meeting()
        tf[meet_check]
        
    })
    
    output$mpTopics <- renderPlot({
        mp_check <- mp()
        meet_check <- meeting2()
        plot <- mp_treemaps[[meet_check]][mp_check]
        if(nrow(plot[[1]]$data) > 0) {
            plot[mp_check]
        } else {
            text = "No tagged topical words for this MP/Meeting Combination."
            ggplot() + 
                ggplot2::annotate("text", x = 4, y = 25, size=6, label = text) + 
                theme_bw() +
                theme(panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),
                      axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank())
        }
        
    })
    
    output$mpTable <- renderDT({
        mp_check <- mp()
        meet_check <- meeting2()
        meet_check <- case_when(meet_check == 7 ~ c(1,2,3,4,5,6),
                                T ~ meet_check)
        if(length(unique(meet_check)) > 1) {
            
            df <- mp_tables %>%
                filter(name == mp_check) %>%
                group_by(stemmed) %>%
                mutate(n = sum(n)) %>%
                ungroup() %>%
                select(stemmed, n) %>%
                distinct() %>%
                arrange(desc(n))
            names(df) <- c("stemmed_word", "count")
            df
        }
        else {
            df <- mp_tables %>%
                filter(name == mp_check,
                       id %in% meet_check) %>%
                arrange(desc(n)) %>%
                ungroup() %>%
                select(id, stemmed, n) %>%
                mutate(id = case_when(id == 1 ~ 1,
                                      id == 2 ~ 3,
                                      id == 3 ~ 4,
                                      id == 4 ~ 5,
                                      id == 5 ~ 7,
                                      id == 6 ~ 8))
            names(df) <- c("meeting", "stemmed word", "count")
            df <- df %>%
                select(-meeting)
            df
        }
        
    })
    
    output$kwicTable <- renderTable({
        
        mp_check <- mp()
        meet_check <- meeting2()
        meet_check <- case_when(meet_check == 7 ~ c(1,3,4,5,7,8),
                                meet_check == 1 ~ 1,
                                meet_check == 2 ~ 3,
                                meet_check == 3 ~ 4,
                                meet_check == 4 ~ 5,
                                meet_check == 5 ~ 7,
                                meet_check == 6 ~ 8)
        if(mp_check %in% c("Conservative", "Liberal", "NDP", "Bloc Québécois", "Green Party")) {
            df <- kwic_text %>%
                filter(caucus == mp_check)

            temp <- as.data.frame(kwic(df$comments, input$keyword, window = 1000))
            temp$meeting <- df$id[as.integer(gsub("[a-z]", "",temp$docname))]
            temp$name <- df$name[as.integer(gsub("[a-z]", "",temp$docname))]
            temp$comment <- gsub('.{1}$', '', paste(temp$pre, temp$keyword, temp$post))
            temp$comment <- rm_white(temp$comment)
            temp$meeting <- as.character(case_when(temp$meeting == 1 ~ 1,
                                      temp$meeting == 2 ~ 3,
                                      temp$meeting == 3 ~ 4,
                                      temp$meeting == 4 ~ 5,
                                      temp$meeting == 5 ~ 7,
                                      temp$meeting == 6 ~ 8))
            temp %>%
                filter(meeting %in% meet_check) %>%
                select(meeting, name, comment) %>%
                distinct()
        }
        else {
            df <- kwic_text %>%
                filter(name == mp_check)   

            temp <- as.data.frame(kwic(df$comments, input$keyword, window = 1000))
            temp$meeting <- df$id[as.integer(gsub("[a-z]", "",temp$docname))]
            temp$comment <- gsub('.{1}$', '', paste(temp$pre, temp$keyword, temp$post))
            temp$comment <- rm_white(temp$comment)
            temp$meeting <- as.character(case_when(temp$meeting == 1 ~ 1,
                                      temp$meeting == 2 ~ 3,
                                      temp$meeting == 3 ~ 4,
                                      temp$meeting == 4 ~ 5,
                                      temp$meeting == 5 ~ 7,
                                      temp$meeting == 6 ~ 8))
            temp %>%
                filter(meeting %in% meet_check) %>%
                select(meeting, comment) %>%
                distinct()
        }
        
    })
    
    output$topic_sentiment <- renderPlot({
        mp_check <- mp2()
        top <- topic()
        df <- topic_line_all %>%
            filter(topic %in% top)
        df <- df %>%
            ungroup() %>%
            mutate(id = case_when(id == 1 ~ "Meeting 1",
                                  id == 2 ~ "Meeting 3",
                                  id == 3 ~ "Meeting 4",
                                  id == 4 ~ "Meeting 5",
                                  id == 5 ~ "Meeting 7",
                                  id == 6 ~ "Meeting 8"))
        
        df2 <- as.data.frame(individual_barplots[[mp_check]])
        names(df2) <- c("topic", "id", "n", "color")
        
        df2 <- df2 %>%
            group_by(topic, id) %>%
            count() %>%
            mutate(color = case_when(topic == "trade" ~ "#606c38",
                                     topic == "health" ~ "#283618",
                                     topic == "rights" ~ "#DA9587",
                                     topic == "democracy" ~ "#dda15e",
                                     topic == "consular" ~ "#bc6c25",
                                     topic == "relations" ~ "#5e6472",
                                     topic == "security" ~ "black")) %>%
            ungroup() %>%
            mutate(id = case_when(id == 1 ~ "Meeting 1",
                                  id == 2 ~ "Meeting 3",
                                  id == 3 ~ "Meeting 4",
                                  id == 4 ~ "Meeting 5",
                                  id == 5 ~ "Meeting 7",
                                  id == 6 ~ "Meeting 8")) %>%
            filter(topic %in% top)
        
        ggplot(df, aes(x = as.factor(id), y = n, color = color, group = topic)) +
            geom_line(size =  1.2) +
            scale_color_identity(guide = "legend", labels = unique(df$topic), breaks = unique(df$color)) +
            geom_col(data = df2, aes(x = as.factor(id), y = n, fill = color), width = 0.3) +
            scale_fill_identity(guide = "legend", labels = unique(df2$topic), breaks = unique(df2$color))
    })
    
    output$topic_sentiment_mp <- renderPlot({
        mp_check <- mp2()
        top <- topic()
        df <- topic_line_mp %>%
            filter(topic %in% top)
        df <- df %>%
            ungroup() %>%
            mutate(id = case_when(id == 1 ~ "Meeting 1",
                           id == 2 ~ "Meeting 3",
                           id == 3 ~ "Meeting 4",
                           id == 4 ~ "Meeting 5",
                           id == 5 ~ "Meeting 7",
                           id == 6 ~ "Meeting 8"))
        df2 <- as.data.frame(individual_barplots[[mp_check]])
        names(df2) <- c("topic", "id", "n", "color")
        
        df2 <- df2 %>%
            mutate(id = case_when(id == 1 ~ "Meeting 1",
                                  id == 2 ~ "Meeting 3",
                                  id == 3 ~ "Meeting 4",
                                  id == 4 ~ "Meeting 5",
                                  id == 5 ~ "Meeting 7",
                                  id == 6 ~ "Meeting 8")) %>%
            filter(topic %in% top)
        
        ggplot(df, aes(x = as.factor(id), y = n, color = color, group = topic)) +
            geom_line(size =  1.2) +
            scale_color_identity(guide = "legend", labels = unique(df$topic), breaks = unique(df$color)) +
            geom_col(data = df2, aes(x = as.factor(id), y = n, fill = color), width = 0.3) +
            scale_fill_identity(guide = "legend", labels = unique(df2$topic), breaks = unique(df2$color)) +
            labs(subtitle = "The line graph shows total topic use over time. The bar graph shows topic use by the selected MP / caucus over time.")
            
    })
    
    
    output$topic_kwic_table <- renderTable({
        top <- topic()
        mp_check <- mp2()
        meet_check <- meet_filter_kwic()
        keywords <- dict %>%
            filter(topic %in% top)
        keywords <- keywords$token

        if(mp_check %in% c("Conservative", "Liberal", "NDP", "Bloc Québécois", "Green Party")) {
            df <- kwic_text %>%
                filter(caucus == mp_check)
            temp <- as.data.frame(kwic(df$comments, keywords, window = 1000))
            temp$meeting <- df$id[as.integer(gsub("[a-z]", "",temp$docname))]
            temp$name <- df$name[as.integer(gsub("[a-z]", "",temp$docname))]
            temp$comment <- gsub('.{1}$', '', paste(temp$pre, temp$keyword, temp$post))
            temp$comment <- rm_white(temp$comment)
            temp$meeting <- as.character(case_when(temp$meeting == 1 ~ 1,
                                      temp$meeting == 2 ~ 3,
                                      temp$meeting == 3 ~ 4,
                                      temp$meeting == 4 ~ 5,
                                      temp$meeting == 5 ~ 7,
                                      temp$meeting == 6 ~ 8))
            temp %>%
                filter(meeting %in% meet_check) %>%
                select(meeting, name, comment) %>%
                distinct()
            
        }
        else {
            df <- kwic_text %>%
                filter(name == mp_check)
            temp <- as.data.frame(kwic(df$comments, keywords, window = 1000))
            temp$meeting <- df$id[as.integer(gsub("[a-z]", "",temp$docname))]
            temp$comment <- gsub('.{1}$', '', paste(temp$pre, temp$keyword, temp$post))
            temp$comment <- rm_white(temp$comment)
            temp$meeting <- as.character(case_when(temp$meeting == 1 ~ 1,
                                                   temp$meeting == 2 ~ 3,
                                                   temp$meeting == 3 ~ 4,
                                                   temp$meeting == 4 ~ 5,
                                                   temp$meeting == 5 ~ 7,
                                                   temp$meeting == 6 ~ 8))
            temp %>%
                filter(meeting %in% meet_check) %>%
                select(meeting, comment) %>%
                distinct()
        }
        
        
    })
    
    
})
