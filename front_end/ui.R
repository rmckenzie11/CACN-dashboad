#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
library(shiny)

## Load in file of MP names
mp_names <- readRDS("data/mp_names.rds")


# Define UI for application that draws a histogram
shinyUI(navbarPage("CACN Dashboard v5",
                   
                   theme = shinytheme("cerulean"),
                   
                   tabPanel("Home",
                            checkboxInput("witness", "Include Witnesses?"),
                            h4("Welcome!"),
                            p("This das`hboard analyzes transcripts from CACN meetings, Jan 20 - Mar 9. In that time, over 100,000 words have been spoken in committee. Use the tabs to navigate between the various features of the dashboard. Once you do, a sidebar will appear that will allow you to customize the displays."),
                            p("The first tab, Meeting Focus, shows word counts, topic counts, and topic modelling for any meeting of the CACN. Use the sidebar options to switch between meetings."),
                            p("The second tab, MP Focus, shows the selected MP's most frequently mentioned topics and their most frequent words. You can also search through their comments in committee using a keyword. The sidebar options allow you to switch between MPs and display data from a selected meeting (or all meetings!)."),
                            p("The third tab, Topic Focus, shows total topic frequency over time, as well as topic frequency for the selected MP. As well, you can see all the selected MP's comments that were tagged as 'topical'. Use the 'Topics' option on the sidebar to display more or different topics, both for the selected MP and total topic frequency."),
                            p("Click the checkbox in the top left if you'd like witness statements included in the visualizations."),
                            p("Future updates: Sentiment analyisis of topics, expanded topic dictionaries, + who knows?"),
                            img(src = "flags.png")
                            
                   ),
                   
                   tabPanel("Meeting Focus",
                            
                            sidebarPanel(
                                h2("Options"),
                                selectInput("meet_select", label = "Meeting:", choices = c("All Meetings", "Jan 20: Election of Chair", "Jan 30: Briefing on bilateral relations and trade", "Feb 4: Briefing on consular issues and extradition", "Feb 5: Appearance by the Ambassador to China", "Feb 24: Briefing from Academics", "Mar 9: Committee Business, appearance by former ambassadors to China")),
                                hr(),
                                helpText("Data from ourcommons.ca")
                            ),
                            mainPanel(
                                conditionalPanel(
                                    condition = "input.witness == 1",
                                    
                                    h4("Word Count"),
                                    ## First Plot is Bar Graph of Words
                                    plotOutput("wordcount_master"),
                                    
                                    h4("Topics Mentioned"),
                                    ## Second Plotly is Topics!
                                    plotOutput("topics_master")),
                                
                                conditionalPanel(
                                    condition = "input.witness == 0",
                                    
                                    ## First Plot is Bar Graph of Words
                                    h4("Word Count"),
                                    plotOutput("wordcount_mp"),
                                    h4("Topics Mentioned"),
                                    plotOutput("topics_mp")),
                                
                                ## Last plot is TF-IDF
                                conditionalPanel(
                                    condition = "input.meet_select != 'All Meetings'",
                                        h4("Topic Modelling"),
                                               h5("TF-IDF", align = "center"),
                                               plotOutput("tf_plot")
                                    )
                            )
                   ),
                   
                   tabPanel("MP Focus",
                            
                            sidebarPanel(
                                h2("Options"),
                                selectInput("mp_select", "MP:", 
                                            choices = mp_names, selected = "Mr. Richard Oliphant"),
                                hr(),
                                selectInput("meet_select2", label = "Meeting:", choices = c("All Meetings", "Jan 20: Election of Chair", "Jan 30: Briefing on bilateral relations and trade", "Feb 4: Briefing on consular issues and extradition", "Feb 5: Appearance by the Ambassador to China", "Feb 24: Briefing from Academics", "Mar 9: Committee Business, appearance by former ambassadors to China")),
                                hr(),
                                helpText("Data from ourcommons.ca")
                            ),
                            mainPanel(
                                ## 1st Output, topics
                                h4("Topics Mentioned Most by MP"),
                                plotOutput("mpTopics"),
                                
                                ## 2nd Output Table: the most frequent words 
                                h4("Most frequent words used by MP"),
                                DT::dataTableOutput("mpTable"),
                                
                                #3rd output table, kwic
                                h4("Keywords in Context"),
                                p("Note: The words used in the above table are stemmed, so searching the comments which contain that stem might require trying multiple versions of the word (e.g. export vs. exports)."),
                                textInput("keyword", "Keyword:", value = "", width = NULL,
                                          placeholder = NULL),
                                tableOutput("kwicTable")
                            )
                   ),
                   tabPanel("Topics Focus",
                            
                            sidebarPanel(
                                h2("Options"),
                                selectInput("mp_select2", "MP:", 
                                            choices = mp_names, selected = "Mr. Richard Oliphant"),
                                hr(),
                                checkboxGroupInput("topic_select", "Topics to Display", c("trade", "health", "rights", "consular", "democracy", "security", "relations"), selected = "trade"),
                                hr(),
                                helpText("Data from ourcommons.ca")
                            ),
                            mainPanel(
                                h4("Topics over Time"),
                                conditionalPanel(
                                    condition = "input.witness == 0",
                                    plotOutput("topic_sentiment_mp")
                                ),
                                conditionalPanel(
                                    condition = "input.witness == 1",
                                    plotOutput("topic_sentiment")
                                ),
                                h4("MP Topical Comments"),
                                selectInput("meet_filter_kwic", label = "Meeting:", choices = c("All Meetings", "Jan 20: Election of Chair", "Jan 30: Briefing on bilateral relations and trade", "Feb 4: Briefing on consular issues and extradition", "Feb 5: Appearance by the Ambassador to China", "Feb 24: Briefing from Academics", "Mar 9: Committee Business, appearance by former ambassadors to China")),
                                tableOutput("topic_kwic_table")
                            )
                   )
)
)



