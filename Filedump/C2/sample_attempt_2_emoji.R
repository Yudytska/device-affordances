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


discordemoji <- ":\\w+:"
corpus$discordemoji <- ifelse(corpus$Platform == "Discord", str_count(corpus$Content, discordemoji), 0)
heartemoji <- "❤"
corpus$only_ji <- ji_count(corpus$Content)
corpus$heartcount <- ifelse(corpus$only_ji == 0, str_count(corpus$Content, heartemoji), 0)


corpus$emoji_count <- corpus$only_ji + corpus$discordemoji + corpus$heartcount
corpus$emoji_count[is.na(corpus$emoji_count)] <- 0
corpus$word_count_with_emoji <- corpus$word_count + corpus$emoji_count


emoji_slice <- corpus %>% group_by(Device, Channel) %>% 
  filter(emoji_count > 0) %>% slice_sample(n = 50)
View(emoji_slice)


setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/")
write_csv2(emoji_slice, "emoji_slice.csv")

emoji_corpus <- read_csv2("emoji_slice.csv")



em_corp2 <- corpus %>%
  group_by(Platform) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2


# Model

corpus <- corpus %>% filter(word_count_with_emoji > 0)

# is it overdispersed?
all_fit<-glmer(emoji_count ~ Device * Platform + (1|Participant) + offset(log(word_count_with_emoji)), family = "poisson",  data=corpus)
summary(all_fit)
dispersion_glmer(all_fit) 


discord_corpus <- corpus %>% filter(Platform == "Discord")
twitter_corpus <- corpus %>% filter(Platform == "Twitter")

t_fit<-glmer(emoji_count ~ Device * Channel + (1|Participant) + offset(log(word_count_with_emoji)), family = "poisson",  data=twitter_corpus)

dispersion_glmer(t_fit) 
summary(t_fit)

d_fit<-glmer(emoji_count ~ Device * Channel + (1|Participant) + offset(log(word_count_with_emoji)), family = "poisson",  data=discord_corpus)

dispersion_glmer(d_fit) 
summary(d_fit)

# Are the top 10 emojis the same?
corpus$emoji_token <- ji_extract_all(corpus$Content)

corpus_em <- unnest(corpus, emoji_token)
emoji_table <- corpus_em %>%
  dplyr::select(c(Device, Platform, emoji_token)) %>%
  group_by(Device, Platform, emoji_token)

emoji_table2 <- emoji_table %>%
  group_by(Device, Platform, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(10)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/")

em_corp2 <- corpus %>%
  group_by(Platform, Device) %>%
  summarise(word_count = sum(word_count_with_emoji))
em_corp2

write_csv2(emoji_table2, "most_frequent_emojis_C2.csv")

emoji_table2 <- emoji_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
emoji_table2$normalised_freq <- round(emoji_table2$normalised_freq)

# Discord-specific

discord_corpus$emoji_token <- str_extract_all(discord_corpus$Content, discordemoji)

corpus_em_d <- unnest(discord_corpus, emoji_token)
emoji_table_d <- corpus_em_d %>%
  dplyr::select(c(Device, Channel, emoji_token)) %>%
  group_by(Device, emoji_token)

emoji_table2_d <- emoji_table_d %>%
  group_by(Device,emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(11)


discord_corpus$ji_token <- ji_extract_all(discord_corpus$Content)
corpus_em_d <- unnest(discord_corpus, emoji_token)
corpus_em_d2 <- unnest(discord_corpus, ji_token)
corpus_em_d2$emoji_token <- corpus_em_d2$ji_token

emoji_table_d1 <- corpus_em_d %>%
  dplyr::select(c( emoji_token, Device)) %>%
  group_by( Device, emoji_token)

emoji_table2_d1 <- emoji_table_d1 %>%
  group_by( Device, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(11)

emoji_table_d2 <- corpus_em_d2 %>%
  dplyr::select(c( emoji_token, Device)) %>%
  group_by(  Device, emoji_token)

emoji_table2_d2 <- emoji_table_d2 %>%
  group_by( Device, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(11)

emoji_table2_d3 <- rbind(emoji_table2_d1, emoji_table2_d2)
emoji_table2_d3 <- emoji_table2_d3 %>% group_by(Device, emoji_token)
write_csv2(emoji_table2_d3, "top_emojis_discord.csv")


# Twitter

twitter_corpus$emoji_token <- ji_extract_all(twitter_corpus$Content)
corpus_em_t <- unnest(twitter_corpus, emoji_token)

emoji_table_t1 <- corpus_em_t %>%
  dplyr::select(c(Channel, emoji_token)) %>%
  group_by(Channel, emoji_token)

emoji_table2_t1 <- emoji_table_t1 %>%
  group_by(Channel, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(10)


emoji_table2_t1 <- emoji_table_t1 %>%
  group_by(Channel, emoji_token) %>%
  summarise(emoji_type_frequency = n()) 


# More than 1 emoji on Discord per channel

discord_corpus$more_emoji <- ifelse(discord_corpus$emoji_count > 1, TRUE, FALSE)
table(discord_corpus$more_emoji, discord_corpus$Device)
twitter_corpus$more_emoji <- ifelse(twitter_corpus$emoji_count > 1, TRUE, FALSE)
table(twitter_corpus$more_emoji, twitter_corpus$Device)

# Type of emoji used


emoji_table2_d1 <- emoji_table_d1 %>%
  group_by(Device, emoji_token) %>%
  summarise(emoji_type_frequency = n()) 
emoji_table2_d1$Device <- as.factor(emoji_table2_d1$Device)
summary(emoji_table2_d1)


emoji_table2_t1 <- emoji_table_t1 %>%
  group_by(Device, emoji_token) %>%
  summarise(emoji_type_frequency = n()) 
emoji_table2_t1$Device <- as.factor(emoji_table2_t1$Device)
summary(emoji_table2_t1)


emoji_table_t1 <- corpus_em_t %>%
  dplyr::select(c(Device, emoji_token)) %>%
  group_by(Device, emoji_token)

emoji_table2_t1 <- emoji_table_t1 %>%
  group_by(Device, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(10)



