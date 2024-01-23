# By person


# Add a test?
library(glmmTMB)
library(AER)
library(blmeco)

library(lme4)
library(tidyverse)
library(tidytext)
library(emo)
library(pscl)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(MASS)

library(foreign)
library(VGAM)
library(boot)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus")
corpus <- read_csv2("work_corpus.csv")
full_corpus <- corpus


firstcapitalletter <- function(x){
  fcl <- str_detect(x, "^[A-Z][^[A-Z]]")
  return(fcl)
}


firstlowercase <- function(x){
  flc <- str_detect(x, "^[a-z]")
  return(flc)
}


clean_urls <- function(x){
  a <- str_replace(x, "http", "URL")
  return(a)
}

corpus$Content <- clean_urls(corpus$Content)
corpus$Capital = apply(corpus[,7], MARGIN = 1, FUN = firstcapitalletter)
corpus$Lowercase = apply(corpus[,7], MARGIN = 1, FUN = firstlowercase)


corpus$Firstletter <- ifelse(corpus$Capital == TRUE, "Uppercase", 
                             ifelse(corpus$Lowercase == TRUE, "Lowercase", NA))

corpus <- subset(corpus, !is.na(Firstletter))
corpus$Firstletter <- factor(corpus$Firstletter, levels = c("Uppercase", "Lowercase"))


corpus <- subset(corpus, Channel == "general")
corpus$Channel <- paste0("#", corpus$Channel, sep="")

# Discord

my_colors2 <- RColorBrewer::brewer.pal(9, "Purples")[c(4,7)]

ggplot(corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Participant") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

corpus %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  group_by( Participant, Device, Firstletter) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n*100/total) %>% 
  filter(Firstletter == "Lowercase")

# ina
corpus <- full_corpus
corpus <- corpus %>% filter(Channel %in% c("general", "broadcast"))
ina <- corpus %>% filter(Participant == "Ina")

my_colors2 <- RColorBrewer::brewer.pal(9, "YlGn")[c(2,4)]
ggplot(ina, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Platform") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

# i for ina
upper_ina <- ina %>% filter(Firstletter == "Uppercase")
upper_ina$i_start <- str_detect(upper_ina$Content, "^[Ii]('|\\s)")
summary(upper_ina)



# tereza

my_colors2 <- RColorBrewer::brewer.pal(9, "YlOrBr")[c(5,7)]
tereza <- corpus %>% filter(Participant == "Tereza")
ggplot(tereza, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Platform") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)


# simone

my_colors2 <- RColorBrewer::brewer.pal(9, "GnBu")[c(6,8)]
simone <- corpus %>% filter(Participant == "simone")
ggplot(roy, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Platform") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)


corpus %>% 
  dplyr::filter(Participant %in% c("Ina", "Roy", "Tereza")) %>% 
  dplyr::filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  group_by(Participant, Channel, Device, Firstletter) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n*100/total) %>% 
  filter(Firstletter == "Uppercase")
