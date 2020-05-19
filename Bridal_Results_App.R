library(shinydashboard)
library(shiny)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

setwd("~/Michelle's Shower")

source <- "1ti-CttrCvS_pslaOyO3fHaDduVxGTtzUDz5yE9E_lpg"

clean <- clean_results(source)
answers <- clean[[1]]
scores <- clean[[2]]
advice <- clean[[3]]

winnerID <- scores %>%
  group_by(Game) %>%
  slice(1:3) %>%
  select(Game, Points) %>%
  unique(.)

winners <- inner_join(scores, winnerID, by = c("Game", "Points"))

ui <- dashboardPage(
  dashboardHeader(title = "Michelle's Bridal Shower Games!",
                  titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    tabsetPanel(
      
      tabPanel("How Well Do You Know the Bride?",
              fluidRow(
                box(
                  title = "Answers",
                  plotOutput("plot1", height = 700),
                  width = 10
                )
              ),
              fluidRow(
                box(
                  title = "Winners",
                  tableOutput("table1"),
                  width = 5
                )
              )
      ),
      
      tabPanel("Bride or Groom? Guess Who Said It",
               fluidRow(
                 box(
                   title = "Answers",
                   plotOutput("plot2", height = 700),
                   width = 10
                 )
               ),
               fluidRow(
                 box(
                   title = "Winners",
                   tableOutput("table2"),
                   width = 6
                 )
               )
      ),

      tabPanel("How Old was the Bride-to-be?",
               fluidRow(
                 box(
                   title = "Answers",
                   plotOutput("plot3", height = 700),
                   width = 10
                 )
               ),
               fluidRow(
                 box(
                   title = "Winners",
                   tableOutput("table3"),
                   width = 6
                 )
               )
      ),

      tabPanel("Would She Rather?",
               fluidRow(
                 box(
                   title = "Answers",
                   plotOutput("plot4", height = 700),
                   width = 10
                 )
               ),
               fluidRow(
                 box(
                   title = "Winners",
                   tableOutput("table4"),
                   width = 6
                 )
               )
      ),

      tabPanel("Emoji Pictionary",
               fluidRow(
                 box(
                   title = "Answers",
                   plotOutput("plot5", height = 700),
                   width = 10
                 )
               ),
               fluidRow(
                 box(
                   title = "Winners",
                   tableOutput("table5"),
                   width = 6
                 )
               )
      ),
      
      tabPanel("Advice",
              h2("Wishes and Advice")
      )
    )
  )
)


server <- function(input, output) {
  
  createPlot <- function(game){
    gg <- ggplot(data = answers %>% filter(Game == game),
                 aes(x = Answer, fill = Correct)) +
      geom_bar() +
      facet_wrap(~Question, scales = "free_y") +
      scale_y_continuous(breaks = pretty_breaks(), name = "") +
      scale_x_discrete(name = "") +
      coord_flip() +
      theme_gray(base_size = 14) +
      theme(legend.title = element_blank(),
            legend.position = "bottom") 
    return(gg)
  }
  
  output$plot1 <- renderPlot(createPlot("How Well Do You Know the Bride?"))
  output$table1 <- renderTable(as.list(winners %>% filter(Game == "How Well Do You Know the Bride?")))
  
  output$plot2 <- renderPlot(createPlot("Bride or Groom? Guess Who Said It"))
  output$table2 <- renderTable(winners %>% filter(Game == "Bride or Groom? Guess Who Said It"))

  output$plot3 <- renderPlot(createPlot("How Old was the Bride-to-be?"))
  output$table3 <- renderTable(winners %>% filter(Game == "How Old was the Bride-to-be?"))

  output$plot4 <- renderPlot(createPlot("Would She Rather?"))
  output$table4 <- renderTable(winners %>% filter(Game == "Would She Rather?"))

  output$plot5 <- renderPlot(createPlot("Emoji Pictionary"))
  output$table5 <- renderTable(winners %>% filter(Game == "Emoji Pictionary"))
}

shinyApp(ui, server)
