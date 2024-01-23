# 3. EMOJIS

discordemoji <- ":\\w+:"
count_emoji <- function(x){
  e <- str_count(x, "U\\+1F") #majority of emojis
  f <- str_count(x, "U\\+2([0-4]|[6-9])") #few more emojis
  f2 <- str_count(x, "U\\+25[ABCF]") #few emoji in 25range
  p <- str_count(x, "U\\+200D") #the combine/plus emoji
  c <- str_count(x, "U\\+1F3F[B-F]") #skincolour
  r <- str_count(x, "\\<U\\+1F3F3\\>\\<U\\+FE0F\\>\\<U\\+200D\\>\\<U\\+1F308\\>")
  return(e + f + f2 - p - c - r)
}


corpus$emoji_count <- ji_count(corpus$Content) + str_count(corpus$Content, discordemoji) + count_emoji(corpus$Content)
corpus$emoji_count[is.na(corpus$emoji_count)] <- 0
corpus$word_count_with_emoji <- corpus$word_count + corpus$emoji_count


em_corp <- corpus %>%
  group_by(Participant, Platform) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp$freq_normalised <- em_corp$emoji_freq * 1000 / em_corp$word_count 
em_corp

ggplot(em_corp, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  #facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants across channels",
       y = "Emoji frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


em_corp2 <- corpus %>%
  group_by(Participant, Channel) %>%
  summarise(emoji_freq = sum(emoji_count),
            word_count = sum(word_count_with_emoji))
em_corp2$freq_normalised <- em_corp2$emoji_freq * 1000 / em_corp2$word_count 
em_corp2

ggplot(em_corp2, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "Emoji frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

