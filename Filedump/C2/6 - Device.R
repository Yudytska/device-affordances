# By device - Leila + Tereza

# First letter capitalisation

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase"))
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone")) %>% 
  filter(Firstletter %in% c("Uppercase", "Lowercase"))

ggplot(discord_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_grid(Channel ~ Participant, scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)

ggplot(twitter_corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  facet_grid(Channel ~ Participant, scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  scale_y_continuous(labels = scales::percent)


# Comment length

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast"))
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone"))

ggplot(corpus, aes(x = Participant, y = word_count, fill = Participant)) +
  facet_wrap("Channel", scales = "free") +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values = my_colors2) +
  labs(y="Comment length (tokens)", x = "Participants over channels") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

ggplot(discord_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  facet_grid(Channel ~ Participant, scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "", fill ="Device") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 30))

ggplot(twitter_corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  facet_grid(Channel ~ Participant, scales = "free") +
  scale_fill_manual(values = my_colors2) +
  theme_clean() +
  labs(y="Comment length (tokens)", x = "", fill ="Device") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 6))


# EMOJI


em_corp2 <- discord_corpus %>%
  group_by(Participant, Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2


ggplot(em_corp2, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_grid(Channel ~ Participant, scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Emoji frequency (per 1,000 tokens)") +
  coord_cartesian(ylim=c(0, 40))

em_corp2 <- twitter_corpus %>%
  group_by(Participant, Channel, Device) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2
ggplot(em_corp2, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_grid(Channel ~ Participant, scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="",
       y = "Emoji frequency (per 1,000 tokens)") +
  coord_cartesian(ylim=c(0, 10))
