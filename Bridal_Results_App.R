library(shinydashboard)
library(shiny)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

setwd("~/Michelle's Shower/Bridal_shower")

source <- "1ti-CttrCvS_pslaOyO3fHaDduVxGTtzUDz5yE9E_lpg"

clean <- clean_results(source)
answers <- clean[[1]]
scores <- clean[[2]]
key_print <- clean[[3]]
advice <- clean[[4]]

answers$Question <- gsub("\n$", "", add_newlines(answers$Question, 30))
answers$Answer <- gsub("\n$", "", add_newlines(answers$Answer, 20))
answers$Guest <- gsub("\n$", "", add_newlines(answers$Guest, 30))

adviceTab <- advice %>%
  filter(Advice_display == "Yes") %>%
  select(-Advice_display)
adviceTab$Advice <- gsub("\n$", "", add_newlines(adviceTab$Advice, 100))
names(adviceTab) <- c("", "")


games <- unique(answers$Game)

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Michelle's Bridal Shower Games!",
                  titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    tabsetPanel(
      
      tabPanel(games[1],
               
               br(),
               fluidRow(
                 column(4,
                   imageOutput("card1", height = 600)
                 ),
                 box(
                   title = "Correct Answers",
                   tableOutput("key1"),
                   width = 4
                   )
                 ),
               br(),
                 
                 fluidRow(
                   box(
                     title = "Guest Answers",
                     girafeOutput("plot1", height = 800),
                     width = 9
                   ),
                   box(
                     title = "Guest Scores",
                     tableOutput("table1"),
                     width = 3
                   )
                 )
               ),
      
      tabPanel(games[2],
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card2", height = 600)
                 ),
                 box(
                   title = "Correct Answers",
                   tableOutput("key2"),
                   width = 3
                 )
               ),
               br(),
               
               fluidRow(
                 box(
                   title = "Guest Answers",
                   girafeOutput("plot2", height = 1000),
                   width = 9
                 ),
                 box(
                   title = "Guest Scores",
                   tableOutput("table2"),
                   width = 3
                 )
               )
      ),

      tabPanel(games[3],
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card3", height = 600)
                 ),
                 box(
                   title = "Correct Answers",
                   tableOutput("key3"),
                   width = 2
                 )
               ),
               br(),
               
               fluidRow(
                 box(
                   title = "Guest Answers",
                   girafeOutput("plot3", height = 1000),
                   width = 9
                 ),
                 box(
                   title = "Guest Scores",
                   tableOutput("table3"),
                   width = 3
                 )
               )
      ),

      tabPanel(games[4],
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card4", height = 600)
                 ),
                 box(
                   title = "Correct Answers",
                   tableOutput("key4"),
                   width = 4
                 )
               ),
               br(),
               
               fluidRow(
                 box(
                   title = "Guest Answers",
                   girafeOutput("plot4", height = 1500),
                   width = 9
                 ),
                 box(
                   title = "Guest Scores",
                   tableOutput("table4"),
                   width = 3
                 )
               )
      ),

      tabPanel(games[5],
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card5", height = 600)
                 ),
                 box(
                   title = "Correct Answers",
                   tableOutput("key5"),
                   width = 4
                 )
               ),
               br(),
               
               fluidRow(
                 box(
                   title = "Guest Answers",
                   girafeOutput("plot5", height = 600),
                   width = 8
                 ),
                 box(
                   title = "Guest Scores",
                   tableOutput("table5"),
                   width = 4
                 )
               )
      ),
      
      tabPanel("Wishes and Advice",
               fluidRow(
                 box(
                   title = "Wishes and Advice",
                   tableOutput("advice"),
                   width = 10
                 )  
               )
      )
    )
  )
)


server <- function(input, output) {
  
  games <- unique(answers$Game)
  cards <- c("Bride_Groom.jpg", "Pictionary.jpg", "How_old.jpg",
             "How_well.jpg", "Would_she_rather.jpg")
  
  output$advice <- renderTable(adviceTab)
  
  ## GAME 1  
  output$plot1 <- renderGirafe({
    gg <- createPlot(answers, games[1])
    girafe(ggobj = gg, pointsize = 14, width_svg = 15, height_svg = 15)
  })
  output$table1 <- renderTable(scores[scores$Game == games[1],
                                            c("Guest", "Points", "Winner")])
  
  output$card1 <- renderImage({
    return(list(src = paste0("./Game_cards/", cards[1]),
                alt = "Card picture",
                height = 600))
    
  }, deleteFile = FALSE)
  
  output$key1 <- renderTable(key_print[key_print$Game == games[1],
                                         c("Question", "Correct Answer")])
  
  ## GAME 2
  output$plot2 <- renderGirafe({
    gg <- createPlot(answers, games[2])
    girafe(ggobj = gg, pointsize = 14, width_svg = 15, height_svg = 15)
  })
  output$table2 <- renderTable(scores[scores$Game == games[2],
                                      c("Guest", "Points", "Winner")])
  
  output$card2 <- renderImage({
    return(list(src = paste0("./Game_cards/", cards[2]),
                alt = "Card picture",
                height = 600))
    
  }, deleteFile = FALSE)
  
  output$key2 <- renderTable(key_print[key_print$Game == games[2],
                                       c("Question", "Correct Answer")])
 
  ## GAME 3
  output$plot3 <- renderGirafe({
    gg <- createPlot(answers, games[3])
    girafe(ggobj = gg, pointsize = 14, width_svg = 15, height_svg = 15)
  })
  output$table3 <- renderTable(scores[scores$Game == games[3],
                                      c("Guest", "Points", "Winner")])
  
  output$card3 <- renderImage({
    return(list(src = paste0("./Game_cards/", cards[3]),
                alt = "Card picture",
                height = 600))
    
  }, deleteFile = FALSE)
  
  output$key3 <- renderTable(key_print[key_print$Game == games[3],
                                       c("Question", "Correct Answer")])
  
  
  ## GAME 4
  output$plot4 <- renderGirafe({
    gg <- createPlot(answers, games[4])
    girafe(ggobj = gg, pointsize = 14, width_svg = 15, height_svg = 15)
  })
  output$table4 <- renderTable(scores[scores$Game == games[4],
                                      c("Guest", "Points", "Winner")])
  
  output$card4 <- renderImage({
    return(list(src = paste0("./Game_cards/", cards[4]),
                alt = "Card picture",
                height = 600))
    
  }, deleteFile = FALSE)
  
  output$key4 <- renderTable(key_print[key_print$Game == games[4],
                                       c("Question", "Correct Answer")])
  
  ## GAME 5
  output$plot5 <- renderGirafe({
    gg <- createPlot(answers, games[5])
    girafe(ggobj = gg, pointsize = 14, width_svg = 15, height_svg = 15)
  })
  output$table5 <- renderTable(scores[scores$Game == games[5],
                                      c("Guest", "Points", "Winner")])
  
  output$card5 <- renderImage({
    return(list(src = paste0("./Game_cards/", cards[5]),
                alt = "Card picture",
                height = 600))
    
  }, deleteFile = FALSE)
  
  output$key5 <- renderTable(key_print[key_print$Game == games[5],
                                       c("Question", "Correct Answer")])

  
}

shinyApp(ui, server)
