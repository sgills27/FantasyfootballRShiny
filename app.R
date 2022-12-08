#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(nflplotR)
library(readr)
library(dplyr)
library(DT)
library(shinythemes)
library(ggdark)


library(readxl)
AllStats <- read_excel("C:/Users/shawn/Downloads/R/FantasyFootballStats/Fantasy_Stats_2021.xlsx",
                      sheet="ALL_Data")
columns = colnames(AllStats)

# Define UI for application 
ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("Fantasy Football Stats 2016-2021"),
                
                # Sidebar for multiple inputs
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Season", "Select Season(s)", AllStats$Season, multiple = TRUE),
                    selectInput("Player", "Select Players(s)", AllStats$Player, multiple = TRUE),
                    selectInput("stat", "Select Statistic(s)", choices = columns)),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Table",
                               DT::dataTableOutput("player_stats")),## table of stats for viewing
                      tabPanel("Graph #1",
                               plotOutput("nflPlot")),
                      tabPanel("Graph #2",
                               plotOutput("distPlot")))),  ## graph for viewing
                     
                )
)



# Define server logic required to display graph and table 
server <- function(input, output) {
  output$player_stats = DT::renderDataTable({
    filter_table = filter(AllStats, Season %in% input$Season & Player %in% input$Player)#creating the data table 
  })
  
  
  output$nflPlot <- renderPlot({
    All_Stats <- filter(AllStats, Player %in% input$Player, Season %in% input$Season) ## creating output for graph
    
    
    ggplot(data = All_Stats, aes(x = Season, y= get(input$stat), z= Player)) +
      geom_line(colour = "red", fill = "white") +
      xlab("Season")+
      ylab("Chosen Stat")+
      dark_theme_classic()+
      ggtitle("Chosen Stat Comparison by Season")+
      theme(plot.title = element_text(hjust = 0.5, size = 18))

  })  
  
  output$distPlot <- renderPlot({
    All_Stats <- filter(AllStats, Player %in% input$Player, Season %in% input$Season) ## creating output for graph
    
    
    ggplot(data = All_Stats, aes(x = Player, y= get(input$stat))) +
      geom_col(fill = "red", colour = "white") +
      xlab("Player")+
      ylab("Chosen Stat")+
      dark_theme_classic()+
      ggtitle("Chosen Stat Comparison")+
      theme(plot.title = element_text(hjust = 0.5, size = 18))
    
  })
    
  
}


## Also define logic to draw season over season bar chart ##


# Run the application 
shinyApp(ui = ui, server = server)
