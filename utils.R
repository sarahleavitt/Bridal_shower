
reload_source <- function(){
  if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
  if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
  if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
  if (!require('naniar')) install.packages('naniar'); library('naniar')
  if (!require('shiny')) install.packages('shiny'); library('shiny')
  if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
  if (!require('DT')) install.packages('DT'); library('DT')
  if (!require('ggiraph')) install.packages('ggiraph'); library('ggiraph')
  if (!require('scales')) install.packages('scales'); library('scales')
  if (!require('googlesheets4')) install.packages('googlesheets4'); library('googlesheets4')
  if (!require('rsconnect')) install.packages('rsconnect'); library('rsconnect')
  
  source("utils.R")
}



clean_results <- function(source = "1ti-CttrCvS_pslaOyO3fHaDduVxGTtzUDz5yE9E_lpg"){
  
  gs4_deauth()
  raw <- read_sheet(source, sheet = "Responses")
  
  clean1 <- raw %>%
    rename(Email = `Email Address`,
           Name = `Your Name`,
           Relation = `Relation to Michelle`,
           Advice = `Advice for the Bride`,
           Advice_display = `Do you want this message to be displayed with the results?`) %>%
    # Changing all factors to character
    mutate_all(as.character) %>%
    replace_na(list(Answer_key = FALSE)) %>%
    mutate(Guest = ifelse(Name != Relation & !is.na(Relation),
                          paste0(Name, " (", Relation, ")"), Name))
  
  clean2 <- clean1[, names(clean1)[!grepl("Would you like to play", names(clean1))]]  
  
  advice <- clean2 %>%
    select(Guest, Advice, Advice_display) %>%
    replace_na(list(Advice_display = "No")) %>%
    filter(!is.na(Advice))
  
  missString <- "no clue|idk|i don't remember|\\?\\?\\?\\?|\\?\\?\\?|great question\\!|no idea"
  
  long <- clean2 %>%
    select(-Email, -Timestamp, -Advice, -Advice_display, -Score) %>%
    pivot_longer(c(-Answer_key, -Name, -Relation, -Guest), names_to = "Question", values_to = "Answer") %>%
    filter(!is.na(Answer)) %>%
    mutate(Game = ifelse(grepl(" I | I'm ", Question), "Bride or Groom? Guess Who Said It",
                         ifelse(grepl("Question", Question), "Emoji Pictionary",
                                ifelse(grepl("Picture", Question), "How Old was the Bride-to-be?",
                                       ifelse(grepl("Would Michelle", Question), "Would She Rather?",
                                              "How Well Do You Know the Bride?")))),
           Answer = ifelse(Game %in% c("How Well Do You Know the Bride?",
                                       "Emoji Pictionary"), gsub(",", "", tolower(Answer)), Answer),
           Answer = ifelse(Answer == "bride-to-be", "bride to be", Answer),
           Number = as.numeric(gsub("[^0-9]", "", Question))) %>%
    mutate(Answer = gsub(missString, "", Answer)) %>%
    replace_with_na(list(Answer = ""))
    
  
  how_well <- long %>%
    filter(Game == "How Well Do You Know the Bride?") %>%
    arrange(desc(Answer_key), Question) %>%
    select(Game, Question, Answer) %>%
    filter(!duplicated(Answer))
  
  
  # Reading in key to complex answers
  key_complex <- read_sheet(source, sheet = "Key") %>%
    mutate(Key = as.character(Key))
  
  key <- long %>%
    filter(Answer_key == TRUE) %>%
    select(Game, Question, Key = Answer) %>%
    filter(!Game %in% c("How Well Do You Know the Bride?", "Emoji Pictionary")) %>%
    mutate(Correct = Key) %>%
    bind_rows(key_complex)
  
  key_print <- key %>%
    filter(!duplicated(Question)) %>%
    select(Game, Question, `Correct Answer` = Key)
  
  answers <- long %>%
    filter(Answer_key == FALSE) %>%
    select(-Answer_key) %>%
    left_join(key, by = c("Game", "Question", "Answer" = "Correct")) %>%
    filter(!is.na(Answer)) %>%
    mutate(Correct = ifelse(!is.na(Key), "Correct!", "Incorrect")) %>%
    select(-Key) %>%
    arrange(Game, Number)
  
  scores <- answers %>%
    filter(Correct == "Correct!") %>%
    group_by(Game, Guest) %>%
    summarize(Points = n()) %>%
    arrange(Game, desc(Points))
  
  hosts <- c("Sarah Leavitt (Sister)", "Julia (Sister in-law)", "Mommy!", "Grandma")
  
  # Finding winners
  winnerID <- scores %>%
    group_by(Game) %>%
    filter(!Guest %in% hosts) %>%
    slice(1) %>%
    select(Game, Points) %>%
    unique(.)
  
  winners <- scores %>%
    inner_join(winnerID, by = c("Game", "Points")) %>%
    mutate(Winner = "Winner!")
  
  scores2 <- scores %>%
    full_join(winners, by = c("Game", "Guest", "Points")) %>%
    mutate(Winner = ifelse(Guest %in% hosts, "Host", Winner)) %>%
    replace_na(list(Winner = ""))
  
  return(list(answers, scores2, key_print, advice, how_well))
}


createPlot <- function(df, game){
  
  # Ordering questions
  plotData <- df %>% filter(Game == game)
  questions <- unique(plotData$Question)
  
  plotData <- plotData %>%
    mutate(Questionf = factor(Question, levels = c(questions[1],questions[2],questions[3],
                                                   questions[4],questions[5],questions[6],
                                                   questions[7],questions[8],questions[9],
                                                   questions[10],questions[11],questions[12],
                                                   questions[13],questions[14],questions[15])))
  gg <- ggplot(data = plotData,
               aes(x = Answer, fill = Correct)) +
    geom_bar() +
    facet_wrap(~Questionf, scales = "free_y", ncol = 3) +
    scale_y_continuous(breaks = pretty_breaks(), name = "") +
    scale_x_discrete(name = "") +
    scale_fill_manual(breaks = c("Correct!", "Incorrect"),
                       values = c("#FF689F", "darkgrey")) +
    coord_flip() +
    theme_gray(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
  
  return(gg)
}


new_lines_adder <- function(test.string, interval) {
  #split at spaces
  string.split = strsplit(test.string," ")[[1]]
  # get length of snippets, add one for space
  lens <- nchar(string.split) + 1
  # now the trick: split the text into lines with
  # length of at most interval + 1 (including the spaces)
  lines <- cumsum(lens) %/% (interval + 1)
  # construct the lines
  test.lines <- tapply(string.split,lines,function(line)
    paste0(paste(line,collapse=" "),"\n"),simplify = TRUE)
  # put everything into a single string
  result <- paste(test.lines,collapse="")
  return(result)
}

add_newlines <- function(x, interval) {
  
  # make sure, x is a character array   
  x = as.character(x)
  # apply splitter to each
  t = sapply(x, FUN = new_lines_adder, interval = interval,USE.NAMES=FALSE)
  return(t)
}

save_results <- function(source = "1ti-CttrCvS_pslaOyO3fHaDduVxGTtzUDz5yE9E_lpg"){
  
  clean <- clean_results()
  
  answers <- clean[[1]]
  scores <- clean[[2]]
  key_print <- clean[[3]]
  advice <- clean[[4]] %>% select(-Advice_display)
  how_well <- clean[[5]]
  
  write.csv(answers, "../shower_answers.csv", row.names = FALSE)
  write.csv(scores, "../shower_scores.csv", row.names = FALSE)
  write.csv(key_print, "../shower_key.csv", row.names = FALSE)
  write.csv(advice, "../shower_advice.csv", row.names = FALSE)
  
  gs4_auth()
  write_sheet(how_well, source, sheet = "How_well")
  
}


#deployApp()
