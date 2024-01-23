# Redoing Graphs, Tables, Stats, with different way of tokenizing!
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



# 2. EMOTICONS
e1 <- "\\)+"
e2 <- "\\(+"
e3 <- "\\|+"

e5 <- "o+"
e6 <- "d+"
e7 <- "p+"
e8 <- "0+"

e100 <- paste0(c(e1,e2,e3,e5,e6,e7,e8), collapse="|")

emoticon1 <- paste0(c("(?<![0-9])(&gt;)*[=:;](')*(\\-)*", "(", e100, ")", "(?![0-9A-z])"), collapse="")
emoticon2 <- "(?<![0-9A-z])xd+(?![0-9A-z])"
emoticon3 <- "(?<![0-9A-z])[0o\\*~][._^]+[0o\\*~][;']*(?![0-9A-z])"
emoticon4 <- "(?<![0-9A-z])\\^[._]*\\^(?![0-9A-z])"
emoticon5 <- "&lt;(\\/)*3+"

all_emoticon <- paste0(c(emoticon1, emoticon2, emoticon3, emoticon4, emoticon5), collapse = "|")

var_list <- ls(pattern = "^e")
rm(list = var_list)

discordemoji <- ":\\w+:"
heartemoji <- "❤"


corpus$content_clean <- str_replace_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]", "URL")
corpus$content_clean <- str_replace_all(corpus$content_clean, "#[\\S]+", "HASHTAG")
corpus$content_clean <- str_replace_all(corpus$content_clean, "@[A-z0-9_]+", "USER")
corpus$content_clean <- str_replace_all(corpus$content_clean, all_emoticon, " EMOTICON ")
corpus$content_clean <- ji_replace_all(corpus$content_clean, " EMOJI ")
corpus$content_clean <- ifelse(corpus$Platform == "Discord", str_replace_all(corpus$content_clean, discordemoji, " EMOJI "), corpus$content_clean)
corpus$content_clean <- str_replace_all(corpus$content_clean, heartemoji, " EMOJI ")
corpus$content_clean <- str_replace_all(corpus$content_clean, "☹", " EMOJI ")
corpus$content_clean <- str_replace_all(corpus$content_clean, "☺", " EMOJI ")
corpus$content_clean <- str_replace_all(corpus$content_clean, "🍽", " EMOJI ")
corpus$content_clean <- str_replace_all(corpus$content_clean, "\\>\\<", " EMOTICON ")

corpus$content_clean <- tolower(corpus$content_clean)
# Punctuation!
corpus$content_clean_punctuation <- str_remove_all(corpus$content_clean, "['’]")

corpus$content_clean_punctuation <- str_replace_all(corpus$content_clean_punctuation, "[[:punct:]]+", " PUNCT ")



corpus <- corpus[, !names(corpus) %in% "word_count"]

tidy_corpus <- corpus %>%
  unnest_tokens(word, content_clean_punctuation)

word_count <- tidy_corpus %>% 
  group_by(ID) %>% 
  summarise(word_count = n())

corpus <- merge(corpus,word_count, by  = "ID", all = TRUE)

corpus$word_count <- ifelse(is.na(corpus$Content), 0, corpus$word_count)

View(corpus)



# TOKENS

twitter_corpus <- corpus %>% subset(Platform == "Twitter")
discord_corpus <- corpus %>% subset(Platform == "Discord")
discord_corpus$Channel <- paste0("#",discord_corpus$Channel)

# Descriptive statistics
group_by(twitter_corpus, Channel, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count),
    median = median(word_count),
  )

group_by(discord_corpus, Channel, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count),
    median = median(word_count),
  )

my_colors <- RColorBrewer::brewer.pal(9, "Purples")[c(4,7)]


ggplot(twitter_corpus, aes(x = Platform, y = word_count)) +
  facet_wrap("Channel") +
  geom_boxplot(aes (fill = Device),position=position_dodge(.9),outlier.shape = NA, show.legend = TRUE) +
  stat_summary(fun=mean, geom="point",  aes(group = Device),position=position_dodge(.9),shape=21, size=4, color="black", fill="#38485A") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 75)) +
  scale_fill_manual(values = my_colors) +
  labs(y="Message length (tokens)", x = "") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

ggplot(discord_corpus, aes(x = Platform, y = word_count)) +
  facet_wrap("Channel") +
  geom_boxplot(aes (fill = Device),position=position_dodge(.9),outlier.shape = NA, show.legend = TRUE) +
  stat_summary(fun=mean, geom="point",  aes(group = Device),position=position_dodge(.9),shape=21, size=4, color="black", fill="#38485A") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 32)) +
  scale_fill_manual(values = my_colors) +
  labs(y="Message length (tokens)", x = "", show.legend = FALSE) +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



p_mfit<-glmer.nb(word_count ~ Device * Platform + (1|Participant),  data=corpus)
summary(p_mfit)


p_mfit<-glmer.nb(word_count ~ Device * Channel + (1|Participant),  data=twitter_corpus)
summary(p_mfit)


p_mfit<-glmer.nb(word_count ~ Device * Channel + (1|Participant),  data=discord_corpus)
summary(p_mfit)

# Turns


setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/")
turn_corpus <- read_csv2("turn_sample.csv")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn, "https?://\\S*[^\\.,:;\"\'\\s]", "URL")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, "#[\\S]+", "HASHTAG")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, "@[A-z0-9_]+", "USER")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, all_emoticon, " EMOTICON ")
turn_corpus$turn_clean <- ifelse(turn_corpus$Platform == "Discord", str_replace_all(turn_corpus$turn_clean, discordemoji, " EMOJI "), turn_corpus$turn_clean)
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, heartemoji, " EMOJI ")
turn_corpus$turn_clean <- ji_replace_all(turn_corpus$turn_clean, " EMOJI ")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, "☹", " EMOJI ")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, "☺", " EMOJI ")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, "🍽", " EMOJI ")
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, "\\>\\<", " EMOTICON ")

turn_corpus$turn_clean <- tolower(turn_corpus$turn_clean)
# Punctuation!
turn_corpus$turn_clean_punctuation <- str_remove_all(turn_corpus$turn_clean, "['’]")
turn_corpus$turn_clean_punctuation <- str_replace_all(turn_corpus$turn_clean_punctuation, "[[:punct:]]+", " PUNCT ")
View(turn_corpus)
tidy_turn_corpus <- turn_corpus %>%
  unnest_tokens(word, turn_clean_punctuation)
turn_corpus <- turn_corpus[, !names(turn_corpus) %in% "word_count"]
word_count <- tidy_turn_corpus %>% 
  group_by(ID) %>% 
  summarise(word_count = n())
turn_corpus <- merge(turn_corpus,word_count, by  = "ID", all = TRUE)
turn_corpus$word_count <- ifelse(is.na(turn_corpus$Content), 0, turn_corpus$word_count)

View(turn_corpus)

group_by(turn_corpus, Channel) %>%
  summarise(
    count = n(),
    sum = sum(word_count),
    mean = mean(word_count),
    sd = sd(word_count)
  )

# Per user

tereza <- corpus %>% subset(Participant == "Tereza")
tereza <- tereza %>% filter(Channel %in% c("general", "broadcast"))

my_colors2 <- RColorBrewer::brewer.pal(9, "YlOrBr")[c(5,7)]
tereza %>% 
  group_by(Platform, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count),
    median = median(word_count)
  )

ggplot(tereza, aes(x = Platform, y = word_count)) +
  geom_boxplot(aes (fill = Device),position=position_dodge(.9),outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 70)) +
  stat_summary(fun=mean, geom="point",  aes(group = Device),position=position_dodge(.9),shape=21, size=4, color="black", fill="#38485A") +
  scale_fill_manual(values = my_colors2) +
  labs(y="Message length (tokens)", x="") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



# simone

simone <- corpus %>% subset(Participant == "Simone")
simone <- simone %>% filter(Channel %in% c("general", "broadcast"))

my_colors2 <- RColorBrewer::brewer.pal(9, "GnBu")[c(6,8)]
simone %>% 
  group_by(Platform, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count),
    median = median(word_count)
  )

ggplot(simone, aes(x = Platform, y = word_count)) +
  geom_boxplot(aes (fill = Device),position=position_dodge(.9),outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 70)) +
  stat_summary(fun=mean, geom="point",  aes(group = Device),position=position_dodge(.9),shape=21, size=4, color="black", fill="#38485A") +
  scale_fill_manual(values = my_colors2) +
  labs(y="Message length (tokens)", x="") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



# ina

ina <- corpus %>% subset(Participant == "Ina")
ina <- ina %>% filter(Channel %in% c("general", "broadcast"))

my_colors2 <- RColorBrewer::brewer.pal(9, "YlGn")[c(2,4)]
ina %>% 
  group_by(Platform, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count),
    median = median(word_count)
  )

ggplot(ina, aes(x = Platform, y = word_count)) +
  geom_boxplot(aes (fill = Device),position=position_dodge(.9),outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 70)) +
  stat_summary(fun=mean, geom="point",  aes(group = Device),position=position_dodge(.9),shape=21, size=4, color="black", fill="#38485A") +
  scale_fill_manual(values = my_colors2) +
  labs(y="Message length (tokens)", x="") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# EMOJI

discordemoji <- ":\\w+:"
corpus$discordemoji <- ifelse(corpus$Platform == "Discord", str_count(corpus$Content, discordemoji), 0)
heartemoji <- "❤"
corpus$only_ji <- ji_count(corpus$Content)
corpus$heartcount <- ifelse(corpus$only_ji == 0, str_count(corpus$Content, heartemoji), 0)


corpus$emoji_count <- corpus$only_ji + corpus$discordemoji + corpus$heartcount
corpus$emoji_count[is.na(corpus$emoji_count)] <- 0
corpus <- corpus[, !names(corpus) %in% c("only_ji", "discordemoji", "heartcount")]


twitter_corpus <- corpus %>% subset(Platform == "Twitter")
discord_corpus <- corpus %>% subset(Platform == "Discord")
discord_corpus$Channel <- paste0("#",discord_corpus$Channel)


em_corp <- twitter_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Channel, fill = Device)) + 
  ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap(~ Channel) +
  theme(legend.position="none", show.legend = FALSE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors) +
  labs(x="",
       y = "Emoji frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


em_corp <- discord_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap(~ Channel) +
  theme(legend.position="none", show.legend = FALSE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors) +
  labs(x="",
       y = "Emoji frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

full_corpus <- corpus
corpus <- corpus %>% filter(word_count > 0)

# is it overdispersed?
all_fit<-glmer(emoji_count ~ Device * Platform + (1|Participant) + offset(log(word_count)), family = "poisson",  data=corpus)
summary(all_fit)
dispersion_glmer(all_fit) 


discord_corpus <- corpus %>% filter(Platform == "Discord")
twitter_corpus <- corpus %>% filter(Platform == "Twitter")

t_fit<-glmer(emoji_count ~ Device * Channel + (1|Participant) + offset(log(word_count)), family = "poisson",  data=twitter_corpus)

dispersion_glmer(t_fit) 
summary(t_fit)

d_fit<-glmer(emoji_count ~ Device * Channel + (1|Participant) + offset(log(word_count)), family = "poisson",  data=discord_corpus)

dispersion_glmer(d_fit) 
summary(d_fit)



# Per user

tereza <- corpus %>% subset(Participant == "Tereza")
tereza <- tereza %>% filter(Channel %in% c("general", "broadcast"))

my_colors2 <- RColorBrewer::brewer.pal(9, "YlOrBr")[c(5,7)]

em_corp <- tereza %>%
  group_by( Platform, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  #theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Emoji frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# simone

simone <- corpus %>% subset(Participant == "Simone")
simone <- simone %>% filter(Channel %in% c("general", "broadcast"))

my_colors2 <- RColorBrewer::brewer.pal(9, "GnBu")[c(6,8)]

em_corp <- simone %>%
  group_by( Platform, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  #theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Emoji frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



# ina

ina <- corpus %>% subset(Participant == "Ina")
ina <- ina %>% filter(Channel %in% c("general", "broadcast"))

my_colors2 <- RColorBrewer::brewer.pal(9, "YlGn")[c(2,4)]


em_corp <- ina %>%
  group_by( Platform, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  #theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Emoji frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# Table for tokens + messages per participant

table_a <- corpus %>% 
  filter(Channel %in% c("broadcast", "general")) %>% 
  filter(Participant %in% c("Tereza", "Simone", "Ina")) %>% 
  group_by(Participant, Platform, Device) %>% 
  summarise(message = n(),
            token = sum(word_count))
write_csv2(table_a, "for_chapter_7.csv")
