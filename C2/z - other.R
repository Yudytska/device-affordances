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
# Save full_corpus for later work.
full_corpus <- corpus

corpus <- full_corpus %>% 
  filter(Platform == "Discord") %>% 
  filter(Participant %in% c("Leila", "Tereza", "Roy", "Ina")) %>% 
  filter(Channel %in%  c("general", "the-tome-zone" ))
corpus$Participant <- as.factor(corpus$Participant)  
corpus$Channel <- as.factor(corpus$Channel)

corpus <- corpus %>% mutate(Participant = factor(Participant, levels = c("Leila", "Roy", "Ina", "Tereza"))) 



corpus <- full_corpus %>% 
  filter(Platform == "Twitter") %>% 
  filter(Participant %in% c("Leila", "Tereza", "Roy", "Ina")) 

corpus$Participant <- as.factor(corpus$Participant)  
corpus$Channel <- as.factor(corpus$Channel)



# 0.1. TUs

# Table of TUs: Device by participant by platform
table(corpus$Participant, corpus$Device)
tbl <- corpus %>% 
  group_by(Platform, Participant, Device) %>% 
  summarise(TUs = n())



# DISCORD

# 0.2.
# Channels by "phone people, computer people"

# Division into phone people
tbl2_phone <- corpus %>% 
  filter(Platform == "Discord") %>% 
  filter(Participant %in% c("Eliza", "Leila", "Michael", "Roy", "Simone")) %>% 
  filter(Channel %in%  c("comics", "general", "sensible-chat", "shout-it-out", "talk-it-out",
                         "the-tome-zone", "the-plague", "tv-and-movies", "games")) %>% 
  group_by(Channel, Participant, Device) %>% 
  summarise(TUs = n())

# Division into computer people
tbl2_computer <- corpus %>% 
  filter(Platform == "Discord") %>% 
  filter(Participant %in% c("Alec", "Fahd", "Ina", "Jonathan", "Nora", "Tereza")) %>% 
  filter(Channel %in%  c("comics", "general", "sensible-chat", "shout-it-out", "talk-it-out",
                         "the-tome-zone", "the-plague", "tv-and-movies", "games")) %>% 
  group_by(Channel, Participant, Device) %>% 
  summarise(TUs = n())

# Phone people graph
ggplot(tbl2_phone, aes(y=TUs, x=Participant, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors) +
  labs(x="Platform",
       y = "TUs") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# Computer people graph
ggplot(tbl2_computer, aes(y=TUs, x=Participant, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors) +
  labs(x="Platform",
       y = "TUs") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# Put into table and CSV file
disc_tbl <- corpus %>% 
  filter(Platform == "Discord") %>% 
  filter(Channel %in% c("comics", "general", "sensible-chat", "shout-it-out", "talk-it-out",
                        "the-tome-zone", "the-plague", "tv-and-movies", "games")) %>%
  filter(Participant %in% c("Ina", "Fahd", "Nora", "Leila", "Roy", "Simone", "Tereza") ) %>% 
  group_by(Channel,Participant, Device) %>% 
  summarise(TUs = n())
write_csv2(disc_tbl, "disc_tbl.csv")


# Table for everyone, Twitter
twit_tbl <- corpus %>% 
  filter(Platform == "Twitter") %>% 
  filter(Participant %in% c("Ina", "Fahd", "Nora", "Leila", "Roy", "Simone", "Tereza") ) %>% 
  group_by(Channel, Participant, Device) %>% 
  summarise(TUs = n())
write_csv2(twit_tbl, "twit_tbl.csv")


# Filter into interesting people to examine
corpus <- corpus %>% filter(Platform == "Discord")
corpus <- corpus %>% filter(Participant %in% c("Eliza", "Ina", "Leila", "Roy", "Tereza", "Jonathan"))
corpus$Type <- ifelse(str_detect(corpus$Participant, "Ina|Tereza|Jonathan"), "Computer", "Phone")
corpus <- corpus %>% mutate(Participant = factor(Participant, levels = c("Eliza", "Leila", "Roy", "Ina", "Tereza", "Jonathan"))) 

corpus <- corpus %>% filter(Participant %in% c( "Leila", "Tereza"))





# IV - exclamation mark


corpus$content_clean <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean <- str_remove_all(corpus$content_clean, "#[\\S]+")
corpus$content_clean <- str_remove_all(corpus$content_clean, "@[A-z0-9_]+")

corpus$single_excl_token <- str_extract_all(corpus$content_clean, "(?<!\\!)\\!(?!\\!+)")
corpus$single_excl_count <- str_count(corpus$content_clean, "(?<!\\.)\\.(?!\\.+)")
corpus$single_excl_count <- str_count(corpus$content_clean, "\\,")


single_excl_corp <- corpus %>% subset(word_count > 0)
single_excl_corp <- single_excl_corp %>%
  group_by(Participant, Channel) %>%
  summarise(single_excl_freq = sum(single_excl_count),
            word_count = sum(word_count))
single_excl_corp$freq_normalised <- single_excl_corp$single_excl_freq * 1000 / single_excl_corp$word_count
single_excl_corp

ggplot(single_excl_corp, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "<,> frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# Ellipsis


corpus$excl_ellipsis_token <- str_extract_all(corpus$content_clean, "(\\!\\!)")
corpus$excl_ellipsis_count <- str_count(corpus$content_clean, "\\?")

excl_ellipsis_corp <- corpus %>% subset(excl_ellipsis_count > 0)
excl_ellipsis_corp <- excl_ellipsis_corp %>%
  group_by(Participant, Channel) %>%
  summarise(excl_ellipsis_freq = sum(excl_ellipsis_count),
            word_count = sum(word_count))
excl_ellipsis_corp$freq_normalised <- excl_ellipsis_corp$excl_ellipsis_freq * 1000 / excl_ellipsis_corp$word_count 
excl_ellipsis_corp


ggplot(excl_ellipsis_corp, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "<.> frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Capitals at all
corpus$character_count <- nchar(corpus$content_clean)

corpus$excl_ellipsis_count <- str_count(corpus$Content, "[A-Z]")

excl_ellipsis_corp <- corpus %>% subset(character_count > 0)
excl_ellipsis_corp <- excl_ellipsis_corp %>%
  group_by(Participant, Channel) %>%
  summarise(excl_ellipsis_freq = sum(excl_ellipsis_count),
            word_count = sum(character_count))
excl_ellipsis_corp$freq_normalised <- excl_ellipsis_corp$excl_ellipsis_freq * 1000 / excl_ellipsis_corp$word_count 
excl_ellipsis_corp


ggplot(excl_ellipsis_corp, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "Capital letter frequency (per 1,000 characters)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



# Single capital words

corpus$excl_ellipsis_count <- str_count(corpus$Content, "\\b[A-Z]{2,}+\\b")

excl_ellipsis_corp <- corpus %>% subset(word_count > 0)
excl_ellipsis_corp <- excl_ellipsis_corp %>%
  group_by(Participant, Channel) %>%
  summarise(excl_ellipsis_freq = sum(excl_ellipsis_count),
            word_count = sum(word_count))
excl_ellipsis_corp$freq_normalised <- excl_ellipsis_corp$excl_ellipsis_freq * 1000 / excl_ellipsis_corp$word_count 
excl_ellipsis_corp


ggplot(excl_ellipsis_corp, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "Capitalised words frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# First letter capitalisation


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

corpus$Firstletter <- as.character(corpus$Firstletter)
corpus$Firstletter[is.na(corpus$Firstletter)] <- "Other"
corpus$Firstletter <- factor(corpus$Firstletter, levels = c("Uppercase", "Lowercase", "Other"))
corpus_fl <- corpus %>% filter(Firstletter %in% c("Uppercase", "Lowercase"))
my_colors2 <- RColorBrewer::brewer.pal(8, "YlGn")[c(4,6)]
ggplot(corpus_fl, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  #facet_wrap("Channel", scales = "free") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)



first_letter <- corpus %>%
  group_by(Participant, Channel, Firstletter) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)
first_letter <- first_letter %>%  filter(Firstletter == "Uppercase")
first_letter$percent <- first_letter$percent * 100

ggplot(first_letter, aes(y=percent, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "First capital letter frequency (in %)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))




c <-table(subcorpus$Firstletter, subcorpus$Device)
d <- prop.table(c, margin = 2)
d

corpus$Firstletter <- as.factor(corpus$Firstletter)
flcmdl <- glm(Firstletter ~ Device, data = corpus, family = "binomial")
summary(flcmdl)
exp(cbind(OR = coef(flcmdl), confint(flcmdl)))

corpus %>%
  group_by(Device, Firstletter) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)
