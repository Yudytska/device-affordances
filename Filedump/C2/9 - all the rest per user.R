# emoji per person

# By person


# Add a test?
library(tidyverse)
library(tidytext)
library(emo)
library(ggthemes)
library(RColorBrewer)

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

corpus <- full_corpus

# tereza

tereza_slice <- tereza %>% filter(Channel == "general") %>% group_by(Device) %>% 
    slice_sample(n = 50)
write_csv2(tereza_slice, "tereza_slice.csv")

# emoji

corpus <- corpus %>% filter(Channel %in% c("general", "broadcast"))
tereza <- corpus %>% filter(Participant == "Tereza")
my_colors2 <- RColorBrewer::brewer.pal(9, "YlOrBr")[c(5,7)]

em_corp <- tereza %>%
  group_by( Platform, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
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

# length
ina$token <- ina$word_count + ina$emoji_count
ina %>% 
  group_by(Platform, Device) %>%
  summarise(
    mean = mean(token),
    sd = sd(token),
    median = median(token)
  )
ggplot(ina, aes(x = Platform, y = token, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 65)) +
  scale_fill_manual(values = my_colors2) +
  labs(y="Message length (tokens)", x="") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

tereza_slice <- read_csv2("tereza_slice.csv")
ggplot(tereza_slice, aes(turns, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Channel ~ Device, scales = "fixed") +
  theme_clean(base_size = 15)+
  scale_fill_manual(values = my_colors2) +
  labs(x="Number of messages per turn",
       y = "Count") 


# letter iteration
corpus$letter_repeat_count <- str_count(corpus$content_clean, "([a-z])\\1{2,}")

lr_corp <- corpus %>% subset(word_count > 0)
lr_corp <- lr_corp %>%
  group_by(Platform, Device) %>%
  summarise(lr_freq = sum(letter_repeat_count),
            word_count = sum(word_count))
lr_corp$freq_normalised <- lr_corp$lr_freq * 1000 / lr_corp$word_count 
lr_corp


ggplot(lr_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Letter iteration frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# periods

corpus$period_count <- str_count(corpus$content_clean, "(?<!\\.)\\.(?!\\.+)")


per_corp <- corpus %>% subset(word_count > 0)
per_corp <- per_corp %>%
  group_by(Platform, Device) %>%
  summarise(per_freq = sum(period_count),
            word_count = sum(word_count))
per_corp$freq_normalised <- per_corp$per_freq * 1000 / per_corp$word_count 
per_corp


ggplot(per_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Period frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# allcaps
corpus$allcaps_count <- str_count(corpus$Content, "\\b[A-Z]{3,}\\b")
allcaps_corp <- corpus %>% subset(word_count > 0)
allcaps_corp <- allcaps_corp %>%
  group_by(Platform, Device) %>%
  summarise(allcaps_freq = sum(allcaps_count),
            word_count = sum(word_count))
allcaps_corp$freq_normalised <- allcaps_corp$allcaps_freq * 1000 / allcaps_corp$word_count 
allcaps_corp


ggplot(allcaps_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "All-cap frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# simone
my_colors2 <- RColorBrewer::brewer.pal(9, "GnBu")[c(6,8)]
corpus <- corpus %>% filter(Channel %in% c("general", "broadcast"))
corpus <- corpus %>% filter(Participant == "Simone")

em_corp <- corpus %>%
  group_by( Platform, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
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

simone$emoji_token <- ji_extract_all(simone$Content)

corpus_em <- unnest(simone, emoji_token)
emoji_table <- corpus_em %>%
  dplyr::select(c(Device, Platform, emoji_token)) %>%
  group_by(Device, Platform, emoji_token)

emoji_table2 <- emoji_table %>%
  group_by(Device, Platform, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(10)

# length

corpus %>% 
  group_by(Platform, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count),
    median = median(word_count)
  )


corpus %>% 
  group_by(Platform) %>% 
  summarise(q = list(quantile(word_count))) %>% 
  unnest_wider(q)
ggplot(corpus, aes(x = Platform, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 62)) +
  scale_fill_manual(values = my_colors2) +
  labs(y="Message length (tokens)", x="") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



simone_slice <- simone %>% filter(Channel == "general") %>% group_by(Device) %>% 
  +  slice_sample(n = 50)
ggplot(simone_slice, aes(turns, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Channel ~ Device, scales = "fixed") +
  theme_clean(base_size = 15)+
  scale_fill_manual(values = my_colors2) +
  labs(x="Number of messages per turn",
       y = "Count") 

# letter iteration
corpus$letter_repeat_count <- str_count(corpus$content_clean, "([a-z])\\1{2,}")

lr_corp <- corpus %>% subset(word_count > 0)
lr_corp <- lr_corp %>%
  group_by(Platform, Device) %>%
  summarise(lr_freq = sum(letter_repeat_count),
            word_count = sum(word_count))
lr_corp$freq_normalised <- lr_corp$lr_freq * 1000 / lr_corp$word_count 
lr_corp


ggplot(lr_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Letter iteration frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# periods

corpus$period_count <- str_count(corpus$content_clean, "(?<!\\.)\\.(?!\\.+)")


per_corp <- corpus %>% subset(word_count > 0)
per_corp <- per_corp %>%
  group_by(Platform, Device) %>%
  summarise(per_freq = sum(period_count),
            word_count = sum(word_count))
per_corp$freq_normalised <- per_corp$per_freq * 1000 / per_corp$word_count 
per_corp


ggplot(per_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Period frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



# ina
my_colors2 <- RColorBrewer::brewer.pal(9, "YlGn")[c(2,4)]
corpus <- corpus %>% filter(Channel %in% c("general", "broadcast"))
corpus <- corpus %>% filter(Participant == "Ina")

ina_slice <- ina %>% filter(Channel == "general") %>% group_by(Device) %>% 
  slice_sample(n = 50)
write_csv2(ina_slice, "ina_slice.csv")
ggplot(ina_slice, aes(turns, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_wrap(~ Device, scales = "fixed") +
  theme_clean(base_size = 15)+
  scale_fill_manual(values = my_colors2) +
  labs(x="Number of messages per turn",
       y = "Count") 
  #xlim(c(0, 15))

em_corp <- corpus %>%
  group_by( Platform, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
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

ina$emoji_token <- str_extract_all(ina$Content, discordemoji)

ina$ji_token <- ji_extract_all(ina$Content)

ina_discord <- ina %>% filter(Platform == "Discord")

corpus_em_d <- unnest(ina_discord, emoji_token)
corpus_em_d2 <- unnest(ina_discord, ji_token)
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



corpus_em <- unnest(simone, emoji_token)
emoji_table <- corpus_em %>%
  dplyr::select(c(Device, Platform, emoji_token)) %>%
  group_by(Device, Platform, emoji_token)

emoji_table2 <- emoji_table %>%
  group_by(Device, Platform, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(10)


# length

corpus %>% 
  group_by(Platform, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count),
    median = median(word_count)
  )
ggplot(corpus, aes(x = Platform, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 60)) +
  scale_fill_manual(values = my_colors2) +
  labs(y="Message length (tokens)", x="") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# letter iteration
corpus$letter_repeat_count <- str_count(corpus$content_clean, "([a-z])\\1{2,}")

lr_corp <- corpus %>% subset(word_count > 0)
lr_corp <- lr_corp %>%
  group_by(Platform, Device) %>%
  summarise(lr_freq = sum(letter_repeat_count),
            word_count = sum(word_count))
lr_corp$freq_normalised <- lr_corp$lr_freq * 1000 / lr_corp$word_count 
lr_corp


ggplot(lr_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Letter iteration frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# periods

corpus$period_count <- str_count(corpus$content_clean, "(?<!\\.)\\.(?!\\.+)")


per_corp <- corpus %>% subset(word_count > 0)
per_corp <- per_corp %>%
  group_by(Platform, Device) %>%
  summarise(per_freq = sum(period_count),
            word_count = sum(word_count))
per_corp$freq_normalised <- per_corp$per_freq * 1000 / per_corp$word_count 
per_corp


ggplot(per_corp, aes(y=freq_normalised, x=Platform, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = TRUE)+
  
  theme(legend.position="none", show.legend = TRUE) +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Period frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))
