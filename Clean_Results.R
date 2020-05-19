
clean_results <- function(source){
  raw <- sheets_read(source, sheet = "Responses")
  
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
    replace_na(list(Advice_display = "No"))
  
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
                                       "Emoji Pictionary"), gsub(",", "", tolower(Answer)), Answer))
  
  how_well <- long %>%
    filter(Game == "How Well Do You Know the Bride?") %>%
    arrange(desc(Answer_key)) %>%
    select(Game, Question, Answer)
  
  write_sheet(how_well, source, sheet = "How_well")
  
  # Reading in key to complex answers
  key_complex <- sheets_read(source, sheet = "Key")
  
  key <- long %>%
    filter(Answer_key == TRUE) %>%
    select(Game, Question, Key = Answer) %>%
    filter(!Game %in% c("How Well Do You Know the Bride?", "Emoji Pictionary")) %>%
    mutate(Correct = Key) %>%
    bind_rows(key_complex)
  
  answers <- long %>%
    filter(Answer_key == FALSE) %>%
    select(-Answer_key) %>%
    left_join(key, by = c("Game", "Question", "Answer" = "Correct")) %>%
    filter(!is.na(Answer)) %>%
    mutate(Correct = ifelse(!is.na(Key), "Correct!", "Incorrect")) %>%
    select(-Key)
  
  scores <- answers %>%
    filter(Correct == "Correct!") %>%
    group_by(Game, Guest) %>%
    summarize(Points = n()) %>%
    arrange(Game, desc(Points))
  
  return(list(answers, scores, advice))
}





