# I. LENGTH OF

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
library(jtools)
library(stringi)

library(glmmTMB)
library(AER)
library(blmeco)


setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/LTC")

corpus <- read_csv2("twitter_big_corpus_clean.csv")


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


corpus$content_clean <- str_replace_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]", "URL")
corpus$content_clean <- str_replace_all(corpus$content_clean, "#[\\S]+", "HASHTAG")
corpus$content_clean <- str_replace_all(corpus$content_clean, "@[A-z0-9_]+", "USER")
corpus$content_clean <- ji_replace_all(corpus$content_clean, " EMOJI ")
# get emoticons from III - Emojis.R
corpus$content_clean <- str_replace_all(corpus$content_clean, all_emoticon, " EMOTICON ")

corpus$content_clean <- tolower(corpus$content_clean)
# Punctuation!
corpus$content_clean_punctuation <- str_remove_all(corpus$content_clean, "['’]")

corpus$content_clean_punctuation <- str_replace_all(corpus$content_clean_punctuation, "[[:punct:]]+", " PUNCT ")

# 1. Comment length (in tokens)

corpus <- corpus[, !names(corpus) %in% "word_count"]

tidy_corpus <- corpus %>%
  unnest_tokens(word, content_clean_punctuation)

word_count <- tidy_corpus %>% 
  group_by(ID) %>% 
  summarise(word_count = n())

corpus <- merge(corpus,word_count, by  = "ID", all = TRUE)

# Descriptive statistics
group_by(corpus, Device) %>%
  summarise(
    count = n(),
    sum = sum(word_count),
    mean = mean(word_count),
    median = median(word_count),
    sd = sd(word_count)
  )

with(corpus, tapply(word_count, Device, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

# Testing??

# linear regression
wcmdl <- lm(word_count ~ Device , data=corpus)
summ(wcmdl, confint = TRUE, digits = 2)
wcmdl <- glm(word_count ~ Device, family=poisson, data = corpus)
summ(wcmdl, confint = TRUE, digits = 2)

wcmdlnb <- glm.nb(word_count ~ Device, data=corpus)
summary(wcmdlnb)
# overdispersion test:
odTest(wcmdlnb)

ggplot(corpus, aes(word_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of tokens per message",
       y = "Frequency") 


round((wcmdlnb$family$linkinv(-0.3088)-1)*100)
# 27% smaller on phone than on computer!

# Is neg binomial better than poisson? if < 0.05, yes
pchisq(2 * (logLik(wcmdlnb) - logLik(wcmdl)), df = 1, lower.tail = FALSE )

est <- cbind(Estimate = coef(wcmdlnb), confint(wcmdlnb))
exp(est)
# Same as the round one! 

# ZTNB doesn't seem to work. try osmeting else?
wcmdlztnb <- vglm(word_count ~ Device, family = posnegbinomial(), data = corpus)
summary(wcmdlbztnb)
est <- cbind(Estimate = coef(wcmdlztnb), confint(wcmdlztnb))
exp(est)


m1 <- vglm(word_count ~ Device, family = posnegbinomial(), data = corpus)
summary(m1)

# Draw it:
p <- ggplot(corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
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

p

ggplot(corpus, aes(x = Device, y = word_count, fill = Device)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, ) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 60)) +
  stat_summary(fun=mean, geom="point", shape=21, size=4, color="black", fill="#38485A") +
  scale_fill_brewer("Paired") +
  labs(y="Message length (tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))




library(wordcloud)
library(RColorBrewer)
library(viridis)
library(wesanderson)
library(tm)
library(SnowballC)
#Create a vector containing only the text


sample_phone <- corpus %>% filter(Device == "Phone") %>% slice_sample(n = 10000)


text_phone <- sample_phone$content_clean_punctuation


rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
text_phone_clean <- rm_words(text_phone, tm::stopwords("en"))
# Create a corpus  
docs <- Corpus(VectorSource(text_phone_clean))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 

b <- c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6")

wordcloud(words = df$word, freq = df$freq,
          max.words=200, random.order=FALSE,rot.per=0.25,
          scale=c(4,1),
          colors=b)




sample_computer <- corpus %>% filter(Device == "Computer") %>% slice_sample(n = 10000)


text_computer <- sample_computer$content_clean_punctuation


rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
text_computer_clean <- rm_words(text_computer, tm::stopwords("en"))
# Create a corpus  
docs <- Corpus(VectorSource(text_computer_clean))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 

a <- c("#084081", "#0868AC", "#2B8CBE", "#4EB3D3", "#7BCCC4")

wordcloud(words = df$word, freq = df$freq,
          max.words=200, random.order=FALSE,rot.per=0.25,
          scale=c(4,1),
          colors=a)


# Model
corpus$Device <- as.factor(corpus$Device)
corpus$Device <- relevel(corpus$Device, ref = "Phone")

wcmdl <- glm(word_count ~ Device, family=poisson, data = corpus)
dispersiontest(wcmdl)
# YES, data is overdispersed, so you use negbinomial
wcmdlnb <- glm.nb(word_count ~ Device, data=corpus)
summary(wcmdlnb)
1/exp(-0.315609)

# 2. Sentence length (in tokens)


sen_corpus <- corpus %>% 
  unnest_tokens(sentences, content_clean, token = "regex", pattern = "([.!?]+|\n)")
sen_corpus <- subset(sen_corpus, str_detect(sen_corpus$sentences, "[A-z]"))

sen_count <- sen_corpus %>% 
  group_by(ID) %>% 
  summarise(sentences = n())

corpus <- merge(corpus,sen_count, by  = "ID", all = TRUE) 
corpus$sentences[is.na(corpus$sentences)] <- 0
corpus$sentences[corpus$sentences == 0] <- NA

corpus$sentence_length <- corpus$word_count / corpus$sentences

corpus_sentence <- corpus %>% filter(sentence_length > 0) %>% filter( word_count > 0) %>%
  filter(sentences > 0)
group_by(corpus_sentence, Device) %>%
  summarise(
    count = n(),
    mean = mean(sentence_length, na.rm = TRUE),
    sd = sd(sentence_length, na.rm = TRUE),
    Median = median(sentence_length, na.rm = TRUE)
  )

group_by(sen_corpus, Device) %>%
  summarise(
    mean = mean(sen_word_count),
    sd = sd(sen_word_count)
  )


ggplot(corpus_sentence, aes(x = Device, y = sentence_length, fill = Device)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, ) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 25)) +
  stat_summary(fun=mean, geom="point", shape=21, size=4, color="black", fill="#38485A") +
  scale_fill_brewer("Paired") +
  labs(y="Sentence length (word tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# model:
#scmdl <- lm(sentence_length ~ Device , data=corpus)
#summ(scmdl, confint = TRUE, digits = 2)
scmdlnb <- glm.nb(word_count ~ Device + offset(log(sentences)), data=corpus)
summary(scmdlnb)
round((scmdlnb$family$linkinv(-0.1780)-1)*100)



scmdlzt <- vglm(word_count ~ Device + offset(log(sentences)), family = posnegbinomial(), data = corpus)
# zero-truncated
scmdlzt <- vglm(sen_word_count ~ Device, family = posnegbinomial(), data = sen_corpus)
summary(scmdlzt)
#dput(round(coef(scmdlzt),3))
#a <- coef(scmdlzt)
#exp(a)
senest <- cbind(Estimate = coef(scmdlzt), confint(scmdlzt))
exp(senest)


scmdlnb <- glm.nb(sen_word_count ~ Device, data=sen_corpus)
summary(scmldnb)
# overdispersion test:
odTest(scmdlnb)

sen_est <- cbind(Estimate = coef(scmdlnb), confint(scmdlnb))
exp(sen_est)

# Draw it:
q <- ggplot(corpus, aes(x = Device, y = sentence_length, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 25)) +
  scale_fill_brewer("Paired") +
  labs(y="Sentence length (tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

q

# Draw it 2:

sen_corpus$ID <- 1:nrow(sen_corpus)
sen_tidy_corpus <- sen_corpus %>%
  unnest_tokens(word, sentences)

sen_word_count <- sen_tidy_corpus %>% 
  group_by(ID) %>% 
  summarise(sen_word_count = n())

sen_corpus <- merge(sen_corpus,sen_word_count, by  = "ID", all = TRUE)
sen_corpus$sen_word_count[is.na(sen_corpus$sen_word_count)] <- 0
sen_corpus <- sen_corpus %>% subset(sen_word_count > 0)

ggplot(corpus_sentence, aes(sentence_length, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of word tokens per sentence",
       y = "Count") 


# 3. Word length (in characters)

tidy_corpus$character_count <- nchar(tidy_corpus$word)
char_count <- tidy_corpus %>% 
  group_by(ID) %>% 
  summarise(char_count_sum = sum(character_count))
corpus <- merge(corpus,char_count, by  = "ID", all = TRUE) 
corpus$char_count_sum[is.na(corpus$char_count_sum)] <- 0
corpus$average_character_count <- corpus$char_count_sum / corpus$word_count




group_by(corpus, Device) %>%
  summarise(
    count = n(),
    mean = mean(average_character_count, na.rm = TRUE),
    sd = sd(average_character_count, na.rm = TRUE),
    median = median(average_character_count, na.rm = TRUE)
  )
group_by(tidy_corpus, Device) %>%
  summarise(
    mean = mean(character_count),
    sd = sd(character_count),
    median = median(character_count, na.rm = TRUE)
  )

# model:

ccmdlnb <- glm.nb(character_count ~ Device, data=tidy_corpus)
summary(ccmdlnb)
# overdispersion test:
odTest(ccmdlnb)

tidy_corpus <- tidy_corpus[1:1000,]
ccmdlzt <- vglm(character_count ~ Device, family = posnegbinomial(), data = sample_words)
summary(ccmdlzt)
dput(round(coef(ccmdlzt),3))
a <- coef(ccmdlzt)
exp(a)

summary(ccmdlzt)
ccest <- cbind(Estimate = coef(ccmdlzt), confint(ccmdlzt))
exp(ccest)

sample_words <- tidy_corpus %>% filter(character_count > 0) %>%  group_by(Device) %>% slice_sample(n = 500000)
ccmdlzt <- vglm(character_count ~ Device, family = posnegbinomial(), data = sample_words)
summary(ccmdlzt)
cc_est <- cbind(Estimate = coef(ccmdlzt), confint(ccmdlzt))


# Clean R enviro first:
gc()
memory.limit(9999999999)

cc_est <- cbind(Estimate = coef(ccmdlnb), confint(ccmdlnb))
exp(cc_est)
cc_est <- confint(ccmdlnb)
exp(cc_est)

# Draw it:
r <- ggplot(corpus, aes(x = Device, y = average_character_count, fill = Device)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 10)) +
  scale_fill_brewer("Paired") +
  labs(y="Token length (characters)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

r


ggplot(tidy_corpus, aes(character_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of characters per word token",
       y = "Count") +
  xlim(c(0, 20))



ggplot(tidy_corpus, aes(x = Device, y = character_count, fill = Device)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, ) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 10)) +
  stat_summary(fun=mean, geom="point", shape=21, size=4, color="black", fill="#38485A") +
  scale_fill_brewer("Paired") +
  labs(y="Word token length (characters)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Draw 2:

tidy_corpus$character_count[is.na(tidy_corpus$character_count)] <- 0
tidy_corpus <- tidy_corpus %>% subset(character_count > 0)

p <- ggplot(tidy_corpus, aes(character_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of characters per token",
       y = "Count") 
p +
  xlim(c(0, 20))

# Look at longest words: >8

w_table <- tidy_corpus %>%
  dplyr::select(c(Device, word, character_count)) %>%
  group_by(Device, word)

w_long_table <- w_table %>% subset(character_count > 8)

w_table2 <- w_long_table %>%
  dplyr::select(c(Device, word)) %>%
  group_by(Device, word) %>%
  summarise(word_type_frequency = n()) %>%
  top_n(50)
w_table2$normalised_freq <- ifelse(w_table2$Device == "Phone", w_table2$word_type_frequency * 1000 / 10897628  , 
                                   w_table2$word_type_frequency * 1000 / 3531794)
w_table2$normalised_freq <- round(w_table2$normalised_freq)
w_table2 <- w_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
View(w_table2)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Chap 5 - Microlinguistic features v1")
write_csv2(w_table2, "50_frequently_words_over_8.csv")

w_long_table2 <- w_long_table
w_long_table2$word<- gsub('([[:alpha:]])\\1+', '\\1', w_long_table2$word)
w_long_table2 <- w_long_table2 %>% select(c(Device, word))

w_table3 <- w_long_table2 %>%
  group_by(Device, word) %>%
  summarise(word_type_frequency = n()) %>%
  top_n(50)
w_table3$normalised_freq <- ifelse(w_table3$Device == "Phone", w_table3$word_type_frequency * 1000 / 10897628  , 
                                   w_table3$word_type_frequency * 1000 / 3531794)
w_table3$normalised_freq <- round(w_table3$normalised_freq)
w_table3 <- w_table3 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
View(w_table3)

# Look at longest words: >12

w_table <- tidy_corpus %>%
  dplyr::select(c(Device, word, character_count)) %>%
  group_by(Device, word)

w_long_table <- w_table %>% subset(character_count > 12)

w_table2 <- w_long_table %>%
  dplyr::select(c(Device, word)) %>%
  group_by(Device, word) %>%
  summarise(word_type_frequency = n()) %>%
  top_n(50)
w_table2$normalised_freq <- ifelse(w_table2$Device == "Phone", w_table2$word_type_frequency * 1000 / 10897628  , 
                                   w_table2$word_type_frequency * 1000 / 3531794)
w_table2$normalised_freq <- round(w_table2$normalised_freq)
w_table2 <- w_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
View(w_table2)

# What about if we also look at all the artificially lengthened ones (through letter rep)
# Nothing particularly interesting! > LMFAO on phone at place 130
w_long_table2 <- w_long_table
w_long_table2$word<- gsub('([[:alpha:]])\\1+', '\\1', w_long_table2$word)
w_long_table2 <- w_long_table2 %>% select(c(Device, word))

w_table3 <- w_long_table2 %>%
  group_by(Device, word) %>%
  summarise(word_type_frequency = n()) %>%
  top_n(50)
w_table3$normalised_freq <- ifelse(w_table3$Device == "Phone", w_table3$word_type_frequency * 1000 / 10897628  , 
                                   w_table3$word_type_frequency * 1000 / 3531794)
w_table3$normalised_freq <- round(w_table3$normalised_freq)
w_table3 <- w_table3 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
w_table3 <- w_table3 %>% select(c(Device, word, normalised_freq))
w_table3$characters <- nchar(w_table3$word)
View(w_table3)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Chap 5 - Microlinguistic features v1")
write_csv2(w_table3, "50_frequently_words_over_12.csv")
