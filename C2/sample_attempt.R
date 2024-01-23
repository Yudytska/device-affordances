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
corpus <- read_csv2("C2.csv")
full_corpus <- corpus

corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast", "general", "the-tome-zone"))

wcmdlnb <- glm.nb(word_count ~ Device * Platform, data=corpus)
summary(wcmdlnb)

tidy_corpus <- corpus %>%
  unnest_tokens(word, content_clean)

t.test(word_count ~ Device, corpus)

cleaned_tidy_corpus <- tidy_corpus
cleaned_tidy_corpus <- tidy_corpus %>%
  anti_join(get_stopwords())

cleaned_tidy_corpus %>%
  count(word, Device, Platform, sort = TRUE)
library(reshape2)
library(wordcloud)

library(textstem)
cleaned_tidy_corpus$lemma <- lemmatize_words(cleaned_tidy_corpus$word)


cleaned_tidy_words <- cleaned_tidy_corpus %>%
  count(Device, lemma, sort = TRUE)



corpus_tf_idf <- cleaned_tidy_words %>%
  bind_tf_idf(lemma, Device, n)
corpus_tf_idf

corpus_tf_idf %>%
  arrange(desc(tf_idf))

w_table <- cleaned_tidy_corpus %>%
  dplyr::select(c(Channel, Device, lemma)) %>%
  group_by(Channel, Device, lemma) %>%
  summarise(word_type_frequency = n()) %>%
  top_n(21) %>% 
  arrange(desc(word_type_frequency)) %>% 
  arrange(Device) %>% 
  arrange(Channel)


setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/")
write_csv2(w_table, "20_frequently_words2.csv")


cleaned_tidy_corpus %>%
  count(word, sort = TRUE) %>%
  acast(word ~ Device, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


# slice_sample() by group in R 
thematic <- corpus %>% group_by(Device, Channel) %>% slice_sample(n = 50)


t_table <- thematic %>%
  dplyr::select(c(Channel, Device, topic)) %>%
  group_by(Channel, Device, topic) %>%
  summarise(topic_count = n()) %>%
  arrange(desc(topic_count)) %>% 
  arrange(Device) %>% 
  arrange(Channel)

jatidy %>% 
  count(book, word, sort=TRUE)
cleaned_tidy_corpus %>% 
  count(word, Device, sort=TRUE) %>% 
  bind_tf_idf(word, Device, n)%>% 
  arrange(desc(tf_idf))


setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/")
turn_corpus <- read_csv2("turn_sample.csv")

turn_corpus %>% 
  group_by(Channel, Device) %>% 
  summarise(mean = mean(turn_msg), sd = sd(turn_msg), median = median(turn_msg))


turn_corpus$turn_clean <- str_remove_all(turn_corpus$turn, "https?://\\S*[^\\.,:;\"\'\\s]")
turn_corpus$turn_clean <- str_remove_all(turn_corpus$turn_clean, "#[\\S]+")
turn_corpus$turn_clean <- str_remove_all(turn_corpus$turn_clean, "@[A-z0-9_]+")

turn_corpus$turn_clean <- ji_replace_all(turn_corpus$turn_clean, "!")
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
emoticon <- paste0(c(emoticon1, emoticon2, emoticon3, emoticon4, emoticon5), collapse = "|")
turn_corpus$turn_clean <- tolower(turn_corpus$turn_clean)
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, emoticon, "!")
rm(e1, e100, e2, e3, e5, e6, e7, e8, emoticon, emoticon1, emoticon2, emoticon3, emoticon4, emoticon5)
discordemoji <- ":\\w+:"
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, discordemoji, "!")
count_emoji <- function(x){
  e <- str_count(x, "U\\+1F") #majority of emojis
  f <- str_count(x, "U\\+2([0-4]|[6-9])") #few more emojis
  f2 <- str_count(x, "U\\+25[ABCF]") #few emoji in 25range
  p <- str_count(x, "U\\+200D") #the combine/plus emoji
  c <- str_count(x, "U\\+1F3F[B-F]") #skincolour
  r <- str_count(x, "\\<U\\+1F3F3\\>\\<U\\+FE0F\\>\\<U\\+200D\\>\\<U\\+1F308\\>")
  return(e + f + f2 - p - c - r)
}
clean_emoji <- function(x){
  e <- str_replace_all(x,"u[+]000", "u+")
  return(e)
}
turn_corpus$turn <- clean_emoji(turn_corpus$turn)
turn_corpus$turn_clean <- clean_emoji(turn_corpus$turn_clean)
turn_corpus$turn_clean <- str_replace_all(turn_corpus$turn_clean, "\\<u\\+\\w{3,5}\\>", "!")


# 1. Comment length (in tokens)
turn_corpus$ID <- 1:nrow(turn_corpus)
tidy_turn_corpus <- turn_corpus %>%
  unnest_tokens(word, turn_clean)

word_count <- tidy_turn_corpus %>% 
  group_by(ID) %>% 
  summarise(turn_count = n())

turn_corpus <- merge(turn_corpus,word_count, by  = "ID", all = TRUE)
turn_corpus$word_count[is.na(turn_corpus$turn_clean)] <- 0
turn_corpus$turn_count[is.na(turn_corpus$turn_count)] <- 0

# Descriptive statistics
group_by(turn_corpus, Channel, Device) %>%
  summarise(
    count = n(),
    sum = sum(turn_count),
    mean = mean(turn_count),
    median = median(turn_count),
    sd = sd(turn_count)
  )

group_by(turn_corpus, Channel, Device) %>%
  summarise(
    count = n(),
    sum = sum(turn_msg),
    mean = mean(turn_msg),
    median = median(turn_msg),
    sd = sd(turn_msg)
  )
table(turn_corpus$turn_msg, turn_corpus$Device)

general_turn <- turn_corpus %>% filter(Channel == "general")
table(general_turn$turn_msg, general_turn$Device)
book_turn <- turn_corpus %>% filter(Channel == "the-tome-zone")
table(book_turn$turn_msg, book_turn$Device)


wcmdlnb <- glmer.nb(word_count ~ Device * Platform + (1|Participant) , data=turn_corpus)
summary(wcmdlnb)
est <- cbind(Estimate = coef(wcmdlnb), confint(wcmdlnb))
exp(est)

p_mfit<-glmer.nb(turn_count ~ Channel * Device + (1|Participant), data=turn_corpus)
summary(p_mfit)

est <- cbind(Estimate = coef(p_mfit), confint(p_mfit))
exp(est)
p_mfit<-glmer.nb(turn_count ~ Channel * Device + (1|Participant), data=turn_corpus)
summary(p_mfit)
est <- cbind(Estimate = coef(p_mfit), confint(p_mfit))
exp(est)



# Word count
all_fit<-glmer.nb(word_count ~ Device * Platform + (1|Participant),  data=corpus)
summary(all_fit)
dispersion_glmer(all_fit)
est <- (Estimate=coef(all_fit))
exp(est)


all_fit<-glmer(word_count ~ Device * Platform + (1|Participant), family = "poisson",  data=corpus)
summary(all_fit)

all_fit<-glmer.nb(word_count ~ Device * Channel + (1|Participant),  data=corpus)
summary(all_fit)

all_fit<-glmer.nb(word_count ~ Device * Platform + (1|Participant),  data=corpus)
summary(all_fit)


nest_fit <- glmer.nb(word_count ~ Device + (1|Platform/Channel) + (1|Participant), data=corpus)
summary(nest_fit)



discord_corpus <- corpus %>% filter(Platform == "Discord")
twitter_corpus <- corpus %>% filter(Platform == "Twitter")

d_fit<-glmer.nb(word_count ~ Device * Channel + (1|Participant),  data=discord_corpus)
summary(d_fit)

deviance(d_fit)/df.residual(d_fit)
d2_fit<-glmer(word_count ~ Device + (1|Participant), family = "poisson",  data=discord_corpus)

dispersion_glmer(d2_fit) # this is UNDER 1.4, meaning the data is NOT over-dispersed

#d_fit <- lmer(word_count ~ Device * Channel + (1|Participant), data=discord_corpus)


t_fit<-glmer.nb(word_count ~ Device * Channel + (1|Participant),  data=twitter_corpus)
summary(t_fit)


a_fit <- glmer.nb(turn_msg ~ Device * Channel + (1|Participant), data = turn_corpus)
b_fit<-glmer(turn_msg ~ Device * Channel + (1|Participant), family = "poisson",  data=turn_corpus)


a_fit <- glmer.nb(turn_count ~ Device * Channel + (1|Participant), data = turn_corpus)
summary(a_fit)


lineplot.CI( x.factor = turn_corpus$Device, 
             response = turn_corpus$turn_count,
             group = turn_corpus$Channel,
             ci.fun = ciMean,
             xlab = "Device",
             ylab = "TUs with emojis (%)",
             col=c("black","red"),  ### Colors for levels of trace var.
             #pch=c(19, 17, 15),             ### Symbols for levels of trace var.
             fixed=TRUE,                    ### Order by factor order in data
             leg.bty = "o")


lineplot.CI( x.factor = corpus$Platform, 
             response = corpus$word_count,
             group = corpus$Device,
             ci.fun = ciMean,
             xlab = "Device",
             ylab = "TUs with emojis (%)",
             col=c("black","red"),  ### Colors for levels of trace var.
             #pch=c(19, 17, 15),             ### Symbols for levels of trace var.
             fixed=TRUE,                    ### Order by factor order in data
             leg.bty = "o")


ggplot(turn_corpus, aes(x = Device, y = turn_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  facet_wrap("Channel", scales = "free") +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 50)) +
  scale_fill_brewer("Paired") +
  labs(y="Comment length (tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Putting all that aside.

all_fit <- lmer(word_count ~ Device * Platform + (1|Participant), data = corpus)
summary(all_fit)

m1 <- lme(word_count~Device * Platform,random=~1|Participant,data=corpus)
anova(m1)
summary(m1)


m2 <- lme(word_count~Device * Channel,random=~1|Participant,data=discord_corpus)
anova(m2)
summary(m2)

m3 <- lme(word_count~Device * Channel,random=~1|Participant,data=twitter_corpus)
anova(m3)
summary(m3)



# Log transform?
corpus$log_word <- log(1 + corpus$word_count)
hist(corpus$log_word)

m1 <- lme(log_word~Device * Platform,random=~1|Participant,data=corpus)
anova(m1)
summary(m1)


# Turn corpus graph of messages


ggplot(turn_corpus, aes(turn_msg, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Channel ~ Device, scales = "fixed") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Pastel2") +
  labs(x="Number of messages per turn",
       y = "Count") + 
  xlim(c(0, 8))
