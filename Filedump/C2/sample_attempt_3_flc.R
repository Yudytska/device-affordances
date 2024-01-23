# sample_attempt_3_flc

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
  a <- str_replace(x, "https", "URL")
  return(a)
}

corpus$Content <- clean_urls(corpus$Content)
corpus$Capital = apply(corpus[,7], MARGIN = 1, FUN = firstcapitalletter)
corpus$Lowercase = apply(corpus[,7], MARGIN = 1, FUN = firstlowercase)


corpus$Firstletter <- ifelse(corpus$Capital == TRUE, "Uppercase", 
                             ifelse(corpus$Lowercase == TRUE, "Lowercase", NA))

corpus <- subset(corpus, !is.na(Firstletter))
corpus$Firstletter <- factor(corpus$Firstletter, levels = c("Uppercase", "Lowercase"))

twitter_corpus <- corpus %>% filter(Platform == "Twitter")
discord_corpus <- corpus %>% filter(Platform == "Discord")

# model all
flcmdl <- glmer(Firstletter ~ Device * Platform + (1|Participant) , data = corpus, family = "binomial")
summary(flcmdl)
exp(cbind(OR = coef(flcmdl), confint(flcmdl)))


flcmdl_tw <- glmer(Firstletter ~ Device * Channel + (1|Participant) , data = twitter_corpus, family = "binomial")
summary(flcmdl_tw)
exp(cbind(OR = coef(flcmdl_tw), confint(flcmdl_tw)))

flcmdl_di <- glmer(Firstletter ~ Device * Channel + (1|Participant) , data = discord_corpus, family = "binomial")
summary(flcmdl_di)
exp(cbind(OR = coef(flcmdl_di), confint(flcmdl_di)))


corpus %>%
  group_by(Device, Firstletter) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)


# extract


flc_slice <- corpus %>% group_by(Platform, Channel, Device, Firstletter) %>% 
  slice_sample(n = 25)
View(flc_slice)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/")
write_csv2(flc_slice, "flc_slice.csv")

flc_slice$Participant <- as.factor(flc_slice$Participant)
table(flc_slice$Participant, flc_slice$Firstletter)


# What's Up With The Tome Zone

corpus$Participant <- as.factor(corpus$Participant)
corpus$Channel <- as.factor(corpus$Channel)

discord_corpus <- corpus %>% filter(Platform == "Discord")

table(discord_corpus$Participant, discord_corpus$Channel)
discord_corpus <- discord_corpus %>% filter(Participant != "Ina")
# Phone data changes, but computer does not. Ina is NOT the "skew"

# is it the I?
discord_corpus$i_start <- str_detect(discord_corpus$Content, "^[Ii]('|\\s)")
table(discord_corpus$i_start, discord_corpus$Channel)
# NO.


flc_slice2 <- discord_corpus %>% group_by(Channel, Firstletter, Device) %>% 
  slice_sample(n = 50)
write_csv2(flc_slice2, "flc_slice_discord.csv")


# Look at others - for #ttz

corpus$Content <- clean_urls(corpus$Content)
corpus$Capital = apply(corpus[,7], MARGIN = 1, FUN = firstcapitalletter)
corpus$Lowercase = apply(corpus[,7], MARGIN = 1, FUN = firstlowercase)


corpus$Firstletter <- ifelse(corpus$Capital == TRUE, "Uppercase", 
                             ifelse(corpus$Lowercase == TRUE, "Lowercase", "Other"))
discord_corpus <- corpus %>% filter(Platform == "Discord")
discord_corpus$Firstletter <- as.factor(discord_corpus$Firstletter)
prop.table(table(discord_corpus$Firstletter, discord_corpus$Channel), margin=2)

discord_corpus$Participant <- as.factor(discord_corpus$Participant)
table(discord_corpus$Channel, discord_corpus$Participant)
discord_corpus <- discord_corpus %>% filter(Participant != "Alec")
discord_corpus <- discord_corpus %>% filter(Participant != "Eliza")
discord_corpus <- discord_corpus %>% filter(Firstletter%in% c("Uppercase", "Lowercase"))

flc_slice <- discord_corpus %>% group_by(Channel, Participant) %>% 
  slice_sample(n = 50)
prop.table(table(flc_slice$Firstletter, flc_slice$Channel), margin=2)


flc_slice %>% 
  group_by(Channel, Device) %>% 
  summarise(mean = mean(word_count), sd = sd(word_count), median = median(word_count))

flc_slice$word_count_with_emoji <- flc_slice$word_count + flc_slice$emoji_count
flc_slice %>% 
  group_by(Channel, Device) %>% 
  summarise(emoji_freq_normalised = (sum(emoji_count)/sum(word_count_with_emoji)*1000),
            emoji_all = sum(emoji_count))


flc_slice %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  group_by( Channel, Device, Firstletter) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n*100/total) %>% 
  filter(Firstletter == "Lowercase")

flc_slice <- flc_slice %>% filter(Firstletter%in% c("Uppercase", "Lowercase"))
ggplot(flc_slice, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)


# Okay, unbias the people???
discord_corpus <- discord_corpus %>% filter(Participant != "Alec")
discord_corpus <- discord_corpus %>% filter(Participant != "Eliza")
discord_corpus <- discord_corpus %>% group_by(Channel, Participant) %>% 
  slice_sample(n = 50)
twitter_corpus <- twitter_corpus %>% group_by(Channel, Participant) %>% 
  slice_sample(n=100)
