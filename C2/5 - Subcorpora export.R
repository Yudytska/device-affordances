# EXPORT SUBCORPUS

# Phone people vs Computer people


library(tidyverse)
library(tidytext)
library(ggthemes)
library(RColorBrewer)
library(emo)
library(lubridate)

my_colors <- RColorBrewer::brewer.pal(11, "Spectral")[c(3,5)]

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus")
corpus <- read_csv2("C2.csv")
full_corpus <- corpus

corpus <- corpus %>% filter(Participant %in% c("Tereza", "Leila"))


# Uppercase v lowercase


firstcapitalletter <- function(x){
  fcl <- str_detect(x, "^[A-Z][^[A-Z]]")
  return(fcl)
}
firstlowercase <- function(x){
  flc <- str_detect(x, "^[a-z]")
  return(flc)
}
corpus$Capital = apply(corpus[,7], MARGIN = 1, FUN = firstcapitalletter)
corpus$Lowercase = apply(corpus[,7], MARGIN = 1, FUN = firstlowercase)

corpus$Firstletter <- ifelse(corpus$Capital == TRUE, "Uppercase", 
                             ifelse(corpus$Lowercase == TRUE, "Lowercase", "Other"))

tereza_cap_twitter <- corpus %>% 
  filter(Participant == "Tereza") %>% 
  filter(Platform == "Twitter") %>% 
  filter(Firstletter == "Lowercase")

leila_cap_twitter <- corpus %>% 
  filter(Participant == "Leila") %>% 
  filter(Platform == "Twitter") %>% 
  filter(Firstletter == "Lowercase")

tereza_cap_discord_phone <- corpus %>% 
  filter(Participant == "Tereza") %>% 
  filter(Platform == "Discord") %>% 
  filter(Device == "Phone") %>% 
  filter(Firstletter == "Lowercase")

tereza_cap_discord_computer <- corpus %>% 
  filter(Participant == "Tereza") %>% 
  filter(Platform == "Discord") %>% 
  filter(Device == "Computer") %>% 
  filter(Firstletter == "Uppercase")

leila_cap_discord <- corpus %>% 
  filter(Participant == "Leila") %>% 
  filter(Platform == "Discord") %>% 
  filter(Device == "Phone") %>% 
  filter(Firstletter == "Lowercase")

leila_cap_discord <- corpus %>% 
  filter(Participant == "Leila") %>% 
  filter(Platform == "Discord") %>% 
  filter(Device == "Computer") %>% 
  filter(Firstletter == "Uppercase")



write_csv2(corpus, "leila_tereza_capitals.csv")
