# 7 - Next attempt

# ALL
my_colors2 <- RColorBrewer::brewer.pal(9, "Purples")[c(4,7)]

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) 
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) 
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")

# 1. LENGTH


# Discord

ggplot(discord_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 30))

# Twitter

ggplot(twitter_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 65))


# EMOJI

# Discord

em_corp <- discord_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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

# Twitter

em_corp2 <- twitter_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2

ggplot(em_corp2, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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


# FLC

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase"))
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase"))
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")

# Discord

ggplot(discord_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

# Twitter

ggplot(twitter_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)


# 2. LEILA

my_colors2 <- RColorBrewer::brewer.pal(9, "GnBu")[c(6,8)]

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Participant == "Leila")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Participant == "Leila")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")


# LENGTH

# Discord

ggplot(discord_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 20))

# Twitter

ggplot(twitter_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 60))


# EMOJI

# Discord

em_corp <- discord_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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

# Twitter

em_corp2 <- twitter_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2

ggplot(em_corp2, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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


# FLC

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Leila")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Leila")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")

# Discord

ggplot(discord_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

# Twitter

ggplot(twitter_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)


# 3. TEREZA

my_colors2 <- RColorBrewer::brewer.pal(9, "YlOrBr")[c(5,7)]

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Participant == "Tereza")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Participant == "Tereza")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")


# LENGTH

# Discord

ggplot(discord_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 32))

# Twitter

ggplot(twitter_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 65))


# EMOJI

# Discord

em_corp <- discord_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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

# Twitter

em_corp2 <- twitter_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2

ggplot(em_corp2, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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


# FLC

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Tereza")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Tereza")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")

# Discord

ggplot(discord_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

# Twitter

ggplot(twitter_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)


# 4. INA

my_colors2 <- RColorBrewer::brewer.pal(9, "YlGn")[c(2,4)]

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Participant == "Ina")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Participant == "Ina")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")


# LENGTH

# Discord

ggplot(discord_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 25))

ggplot(discord_corpus, aes(word_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_wrap("Channel", scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of tokens per message",
       y = "Count") + 
  xlim(c(0, 60))
# Twitter

ggplot(twitter_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 60))


# EMOJI

# Discord

em_corp <- discord_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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

# Twitter

em_corp2 <- twitter_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2

ggplot(em_corp2, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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


# FLC

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Ina")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Ina")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")

# Discord

ggplot(discord_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

# Twitter

ggplot(twitter_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)



# 4. ROY

my_colors2 <- RColorBrewer::brewer.pal(9, "Greens")[c(5,7)]

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Participant == "Roy")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Participant == "Roy")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")


# LENGTH

# Discord

ggplot(discord_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 40))

# Twitter

ggplot(twitter_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 60))


# EMOJI

# Discord

em_corp <- discord_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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

# Twitter

em_corp2 <- twitter_corpus %>%
  group_by( Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2

ggplot(em_corp2, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE)+
  facet_wrap("Channel") +
  theme(legend.position="none", show.legend = FALSE) +
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


# FLC

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Roy")
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant == "Roy")
discord_corpus$Channel <- paste0("#", discord_corpus$Channel, sep="")

# Discord

ggplot(discord_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

# Twitter

ggplot(twitter_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_wrap("Channel") +
  scale_fill_manual(values = my_colors2) +
  theme_clean(base_size = 15) +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)



# TABLES

# LENGTH

discord_corpus %>% 
  group_by(Channel, Device) %>% 
  summarise(mean = mean(word_count), sd = sd(word_count), median = median(word_count))


discord_corpus %>% 
  filter(Participant %in% c("Leila", "Ina", "Roy", "Tereza")) %>% 
  group_by(Participant, Channel, Device) %>% 
  summarise(mean = mean(word_count), sd = sd(word_count), median = median(word_count))


twitter_corpus %>% 
  filter(Platform == "Twitter") %>% 
  group_by(Channel, Device) %>% 
  summarise(mean = mean(word_count), sd = sd(word_count), median = median(word_count))

twitter_corpus %>% 
  filter(Platform == "Twitter") %>% 
  filter(Participant %in% c("Leila", "Ina", "Roy", "Tereza")) %>% 
  group_by(Participant, Channel, Device) %>% 
  summarise(mean = mean(word_count), sd = sd(word_count), median = median(word_count))


# EMOJI

discord_corpus %>% 
  group_by(Channel, Device) %>% 
  summarise(emoji_freq_normalised = (sum(emoji_count)/sum(word_count_with_emoji)*1000),
            emoji_all = sum(emoji_count))

discord_corpus %>% 
  filter(Participant %in% c("Leila", "Ina", "Roy", "Tereza")) %>% 
  group_by(Participant, Channel, Device) %>% 
  summarise(emoji_freq_normalised = (sum(emoji_count)/sum(word_count_with_emoji)*1000),             emoji_all = sum(emoji_count))


twitter_corpus %>% 
  filter(Platform == "Twitter") %>% 
  group_by(Channel, Device) %>% 
  summarise(emoji_freq_normalised = (sum(emoji_count)/sum(word_count_with_emoji)*1000),             emoji_all = sum(emoji_count))

twitter_corpus %>% 
  filter(Platform == "Twitter") %>% 
  filter(Participant %in% c("Leila", "Ina", "Roy", "Tereza")) %>% 
  group_by(Participant, Channel, Device) %>% 
  summarise(emoji_freq_normalised = (sum(emoji_count)/sum(word_count_with_emoji)*1000),             emoji_all = sum(emoji_count))


# CAPITAL


discord_corpus %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  group_by( Channel, Device, Firstletter) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n*100/total) %>% 
  filter(Firstletter == "Lowercase")

discord_corpus %>% 
  dplyr::filter(Participant %in% c("Leila", "Ina", "Roy", "Tereza")) %>% 
  dplyr::filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  group_by(Participant, Channel, Device, Firstletter) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n*100/total) %>% 
  filter(Firstletter == "Lowercase")

twitter_corpus %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  group_by(Channel, Device, Firstletter) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n*100/total) %>% 
  filter(Firstletter == "Lowercase")


twitter_corpus %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase")) %>% 
  filter(Participant %in% c("Leila", "Ina", "Roy", "Tereza")) %>% 
  group_by(Participant, Channel, Device, Firstletter) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n*100/total) %>% 
  filter(Firstletter == "Lowercase")
