library(readr)
library(tidyverse)
library(lubridate)
library(purrr)

setwd("C://users/glumb/documents/university/math513/coursework")

speech_file <- c("BemidjiSep18_2020.txt", "MindenSep12_2020.txt",
                 "FayettevilleSep19_2020.txt",  "MosineeSep17_2020.txt",
                 "FreelandSep10_2020.txt", "OhioSep21_2020.txt",
                 "HendersonSep13_2020.txt", "PittsburghSep22_2020.txt",
                 "LatrobeSep3_2020.txt", "Winston-SalemSep8_2020.txt")


#
# for(i in speech_file){
#   rally_location <- append(str_extract(i, ".+?(?=Sep)"))
# }
#
# for(string in speech_file){
#   print(string)
# }
# find_location_name <- function(txtfile){
#   str_extract(txtfile, ".+?(?=Sep)")
# }
# find_date <- function(txtfile){
#   temp <- str_extract(txtfile, "Sep.*?(?=.txt)")
#   temp <- str_remove(temp, "_")
  # temp <- strptime(temp, "%b%d%Y")
# }



# rally_date <- sapply(speech_file, find_date)
# rally_date <- unname(rally_date)
#
# rally_location <- sapply(speech_file, find_location_name)
# rally_location <- unname(rally_location)
#
# read_speech <- function(file, rally_location, date){
#   import <- read_delim(file, delim = "\t", col_names = "speech")
#   import <- import %>% mutate(location = rally_location,
#                               date = as.Date(date, format = "%Y-%m-%d"))
#   import
# }
# grepl("\\D(\\d{5}$)", test)
#
# test <- "Sep82020"
# if (grepl("\\D(\\d{5}$)", test) == TRUE){
#     print("test")
# }


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

# test <- read_speech_2("Winston-SalemSep8_2020.txt")

p <- lapply(speech_file, read_speech_2)
y <- do.call("rbind", p)
speeches <- y %>% mutate(date = as.Date(date, "%b%d%Y"))



# bemidji <- read_speeech("BemidjiSep18_2020.txt","Bemidji","2020-09-18")
# fayetteville <- read_speech("FayettevilleSep19_2020.txt", "Fayetteville", "2020-09-19")


