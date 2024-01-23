# 1. CATEGORY: LENGTH


corpus %>% 
  group_by(Participant, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count)
  )
ggplot(corpus, aes(x = Participant, y = word_count, fill = Device)) +
  facet_wrap("Type", scales = "free") +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values = my_colors) +
  labs(y="Comment length (tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))




corpus <- corpus %>%  filter(Channel %in% c("comics", "general", "sensible-chat", "shout-it-out", "talk-it-out",
                                            "the-tome-zone", "the-plague", "tv-and-movies", "games"))

my_colors2 <- RColorBrewer::brewer.pal(8, "Accent")[c(1:6)]

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


# BONUS. Do I want to look at platforms - Twitter v Discord?


corpus %>% 
  group_by(Participant, Platform) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count)
  )
ggplot(corpus, aes(x = Participant, y = word_count, fill = Platform)) +
  facet_wrap("Type", scales = "free") +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 60)) +
  scale_fill_manual(values = my_colors) +
  labs(y="Comment length (tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


twitter_corpus <- corpus %>% filter(Platform == "Twitter")
twitter_corpus %>% 
  group_by(Participant, Device) %>%
  summarise(
    mean = mean(word_count),
    sd = sd(word_count)
  )
ggplot(twitter_corpus, aes(x = Participant, y = word_count, fill = Device)) +
  facet_wrap("Type", scales = "free") +
  geom_boxplot(outlier.shape = NA, show.legend = TRUE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 60)) +
  scale_fill_manual(values = my_colors) +
  labs(y="Comment length (tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))
