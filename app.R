
#Loading packages and functions
source("utils.R")
reload_source()

#Getting survey data and cleaning it
clean <- clean_results()

answers <- clean[[1]]
scores <- clean[[2]]
key_print <- clean[[3]]
advice <- clean[[4]]

answers$Question <- gsub("\n$", "", add_newlines(answers$Question, 30))
answers$Answer <- gsub("\n$", "", add_newlines(answers$Answer, 20))

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
      
      tabPanel("Bride or Groom? Guess Who Said It",
               
               br(),
               fluidRow(
                 column(4,
                   imageOutput("card1", height = 600)
                 ),
                 br(),
                 box(
                   title = "Correct Answers",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("key1"),
                   width = 4,
                   )
                 ),
               br(),
               br(),
                 
                 fluidRow(
                   box(
                     girafeOutput("plot1"),
                     width = 9
                   ),
                   br(),
                   box(
                     title = "Guest Scores",
                     status = "success",
                     solidHeader = TRUE,
                     tableOutput("table1"),
                     width = 3
                   )
                 )
               ),
      
      tabPanel("Emoji Pictionary",
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card2", height = 600)
                 ),
                 br(),
                 box(
                   title = "Correct Answers",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("key2"),
                   width = 3
                 )
               ),
               br(),
               br(),
               
               fluidRow(
                 box(
                   girafeOutput("plot2"),
                   width = 9
                 ),
                 br(),
                 box(
                   title = "Guest Scores",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("table2"),
                   width = 3
                 )
               )
      ),

      tabPanel("How Old was the Bride-to-be?",
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card3", height = 600)
                 ),
                 br(),
                 box(
                   title = "Correct Answers",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("key3"),
                   width = 3
                 ),
                 br(),
                 column(5,
                        imageOutput("how_old", height = 600)
                 )
               ),
               br(),
               br(),
               
               fluidRow(
                 box(
                   girafeOutput("plot3"),
                   width = 9
                 ),
                 br(),
                 box(
                   title = "Guest Scores",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("table3"),
                   width = 3
                 )
               )
      ),

      tabPanel("How Well Do You Know the Bride?",
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card4", height = 600)
                 ),
                 br(),
                 box(
                   title = "Correct Answers",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("key4"),
                   width = 4
                 )
               ),
               br(),
               br(),
               
               fluidRow(
                 box(
                   girafeOutput("plot4"),
                   width = 9
                 ),
                 br(),
                 box(
                   title = "Guest Scores",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("table4"),
                   width = 3
                 )
               )
      ),

      tabPanel("Would She Rather?",
               
               br(),
               fluidRow(
                 column(4,
                        imageOutput("card5", height = 600)
                 ),
                 br(),
                 box(
                   title = "Correct Answers",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("key5"),
                   width = 4
                 )
               ),
               br(),
               br(),
               
               fluidRow(
                 box(
                   girafeOutput("plot5"),
                   width = 8
                 ),
                 br(),
                 box(
                   title = "Guest Scores",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("table5"),
                   width = 4
                 )
               )
      ),
      
      tabPanel("Wishes and Advice",
               
               br(),
               fluidRow(
                 column(5,
                        imageOutput("advice_1", height = 450)
                 ),
                 column(4,
                        imageOutput("advice_2", height = 450)
                 ),
               ),
               
               fluidRow(
                 
                 box(
                   title = "Wishes and Advice",
                   status = "success",
                   solidHeader = TRUE,
                   tableOutput("advice"),
                   width = 9
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
  
  ## ADVICE
  output$advice <- renderTable(adviceTab)
  
  output$advice_1 <- renderImage({
    return(list(src = "./Advice_graphic_1.jpg",
                alt = "Advice graphic 1",
                height = 400))
    
  }, deleteFile = FALSE)
  
  output$advice_2 <- renderImage({
    return(list(src = "./Advice_graphic_2.jpg",
                alt = "Advice graphic 2",
                height = 400))
    
  }, deleteFile = FALSE)
  
  
  
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
    girafe(ggobj = gg, pointsize = 14, width_svg = 15, height_svg = 28)
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
  
  output$how_old <- renderImage({
    return(list(src = "./How_old_graphic.png",
                alt = "How old graphic",
                height = 600))
    
  }, deleteFile = FALSE)
  
  
  ## GAME 4
  output$plot4 <- renderGirafe({
    gg <- createPlot(answers, games[4])
    girafe(ggobj = gg, pointsize = 14, width_svg = 15, height_svg = 35)
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
