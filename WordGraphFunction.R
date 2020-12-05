library(readr)
library(tidyverse)
library(lubridate)
library(purrr)
library(tidytext)
library(scales)

setwd("C://Users//ELISW//Documents//UNI//Data")

speech_file <- c("BemidjiSep18_2020.txt", "MindenSep12_2020.txt",
                 "FayettevilleSep19_2020.txt",  "MosineeSep17_2020.txt",
                 "FreelandSep10_2020.txt", "OhioSep21_2020.txt",
                 "HendersonSep13_2020.txt", "PittsburghSep22_2020.txt",
                 "LatrobeSep3_2020.txt", "Winston-SalemSep8_2020.txt")


read_speech_2 <- function(file){
  speeches <- read_delim(file, delim = "\t", col_names = "speech")
  location <- str_extract(file, ".+?(?=Sep)")
  date <- str_extract(file, "Sep.*?(?=.txt)")
  date <- str_remove(date, "_")
  speeches <- speeches %>% mutate(location = location,
                                  date = date)
  if (grepl("\\D(\\d{5}$)", speeches$date) == TRUE){
    speeches$date <- gsub("(\\D*)(.*)","\\10\\2", speeches$date)
  }
  speeches
}


p <- lapply(speech_file, read_speech_2)
y <- do.call("rbind", p)
speeches <- y %>% mutate(date = as.Date(date, "%b%d%Y"))

speeches


##Question 1##

## Function that produces a graph depending on the chosen filter word
word_graph <- function(data_frame, filterword)
{
  # breaks the sentences into words
  words <- data_frame %>% unnest_tokens(word, speech)
  
  #removes stop words
  tidy_words <- words %>% anti_join(stop_words)
  
  
  #Counts the amount of times the word was sid and groups it to the day
  
  words <- tidy_words %>% group_by(date) %>% count(word, sort = TRUE)
  
  
  #Get percent of words used
  
  q1 <- words %>%
    mutate(p = n / sum(n)) 
  
  q1
  
  #Apply filter so it only shows the requested words
  
  filtered_words <- filter(q1, word %in% filterword)
  
  #plots graph
  
  filtered_words %>% 
    ggplot(aes(x = date,
               y = p,
               color = word)) +
    geom_point() +
    theme_minimal() +
    geom_smooth(method = "lm", se = FALSE , color = "black") +
    geom_smooth(se = TRUE)+
    facet_wrap(~word , scales = "free") +
    labs(y = "Percentage of words in Donald Trump's rallies", x = "Rally Date", color = "Word", title = "Change of word frequency in Donald Trump's rallies in September 2020") +
    scale_y_continuous(labels = scales::percent)
  
}


#Choose the words you want filtered, can take as much as you want 
filtered_words <- c("money", "america", "obama", "china", "biden", "people")
word_graph(speeches, filtered_words)


