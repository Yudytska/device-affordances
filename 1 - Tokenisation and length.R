library(tidyverse)
library(tidytext)
library(emo)
library(MASS)
library(pscl)
library(broom)
library(VGAM)
library(ggthemes)

# Set working directory to correct folder with setwd()
# Read in corpus as 'corpus' with read_csv2()

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/LTC")
corpus <- read_csv2("twitter_big_corpus_clean.csv")

# 5.1. + 5.2. GET TOKEN COUNT + MESSAGE LENGTH

# 1. Set up:
# Replacing emoticons, emoji, hashtags, URLs, users, and punctuation with word tokens for tidytext.

# URLs
corpus$content_clean <- str_replace_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]", "URL")
# Emoticons:
corpus$content_clean <- tolower(corpus$content_clean)
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
corpus$content_clean <- str_replace_all(corpus$content_clean, all_emoticon, " EMOTICON ")
# Hashtags
corpus$content_clean <- str_replace_all(corpus$content_clean, "#[\\S]+", "HASHTAG")
# Users
corpus$content_clean <- str_replace_all(corpus$content_clean, "@[A-z0-9_]+", "USER")
# Emoji
corpus$content_clean <- ji_replace_all(corpus$content_clean, " EMOJI ")
# Punctuation
corpus$content_clean_punctuation <- str_replace_all(corpus$content_clean_punctuation, "[[:punct:]]+", " PUNCTUATION ")


# 2. Message length in tokens:
tidy_corpus_token <- corpus %>%
  unnest_tokens(word, content_clean_punctuation)
word_count <- tidy_corpus_token %>% 
  group_by(ID) %>% 
  summarise(token_count = n())

corpus <- merge(corpus,token_count, by  = "ID", all = TRUE)


# 3. Descriptive statistics:

group_by(corpus, Device) %>%
  summarise(
    count = n(),
    sum = sum(token_count),
    mean = mean(token_count),
    median = median(token_count),
    sd = sd(token_count)
  )

# Boxplot
ggplot(corpus, aes(x = Device, y = token_count, fill = Device)) +
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

# Histogram
ggplot(corpus, aes(token_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of tokens per message",
       y = "Frequency") 

# 4. Inferential statistics:

wcmdlnb <- glm.nb(token_count ~ Device, data=corpus)
# overdispersion test:
odTest(wcmdlnb) # Yes, over-dispersed
summary(wcmdlnb)
tidy_results <- tidy(wcmdlnb)
tidy_results$exp_coef <- exp(tidy_results$estimate)
# Adjust the estimates when exp_coef is less than 1
tidy_results$adjusted_exp_coef <- ifelse(tidy_results$exp_coef < 1, 1 / tidy_results$exp_coef, tidy_results$exp_coef)
tidy_results


# SENTENCE CORPUS

# 1. Redo content_clean column with removing URLs, usernames, and hashtags, and replacing emoji and
# emoticons with punctuation. 

# URLs
corpus$content_clean <- str_replace_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]", "")
# Hashtags
corpus$content_clean <- str_replace_all(corpus$content_clean, "#[\\S]+", "")
# Users
corpus$content_clean <- str_replace_all(corpus$content_clean, "@[A-z0-9_]+", "")
# Emoji
corpus$content_clean <- ji_replace_all(corpus$content_clean, " ! ")
# Emoticons:
corpus$content_clean <- str_replace_all(corpus$content_clean, all_emoticon, " ! ")

# Do WORD count now
tidy_corpus <- corpus %>%
  unnest_tokens(word, content_clean)
word_count <- tidy_corpus %>% 
  group_by(ID) %>% 
  summarise(word_count = n())
corpus <- merge(corpus,word_count, by  = "ID", all = TRUE)

# 2. Sentence length in tokens:

# Words per sentence:
sen_corpus <- corpus %>% 
  unnest_tokens(sentences, content_clean, token = "regex", pattern = "([.!?]+|\n)")
sen_corpus <- subset(sen_corpus, str_detect(sen_corpus$sentences, "[A-z]"))
sen_count <- sen_corpus %>% 
  group_by(ID) %>% 
  summarise(sentences = n())
corpus <- merge(corpus,sen_count, by  = "ID", all = TRUE) 
corpus$sentence_length <- corpus$word_count / corpus$sentences

# Reorganise dataframe where each row is one sentence - only sentences with at least one word possible
sen_corpus$ID <- 1:nrow(sen_corpus)
sen_tidy_corpus <- sen_corpus %>%
  unnest_tokens(word, sentences)
sen_word_count <- sen_tidy_corpus %>% 
  group_by(ID) %>% 
  summarise(sen_word_count = n())
sen_corpus <- merge(sen_corpus,sen_word_count, by  = "ID", all = TRUE)
sen_corpus$sen_word_count[is.na(sen_corpus$sen_word_count)] <- 0
sen_corpus <- sen_corpus %>% subset(sen_word_count > 0)

# 3. Descriptive statistics
corpus_sentence <- corpus %>% filter(sentence_length > 0) %>% filter( word_count > 0) %>%
  filter(sentences > 0)
group_by(corpus_sentence, Device) %>%
  summarise(
    count = n(),
    mean = mean(sentence_length, na.rm = TRUE),
    sd = sd(sentence_length, na.rm = TRUE),
    Median = median(sentence_length, na.rm = TRUE)
  )

# Boxplot
ggplot(corpus_sentence, aes(x = Device, y = sentence_length, fill = Device)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, ) +
  theme_clean(base_size = 15) +
  coord_cartesian(ylim=c(0, 22)) +
  stat_summary(fun=mean, geom="point", shape=21, size=4, color="black", fill="#38485A") +
  scale_fill_brewer("Paired") +
  labs(y="Sentence length (word tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))
# Histogram
ggplot(corpus_sentence, aes(sentence_length, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of word tokens per sentence",
       y = "Count") 

# 4. Inferential statistics

scmdlzt <- vglm(sen_word_count ~ Device, family = posnegbinomial(), data = sen_corpus)

# Double-check overdispersion (odTest() doesn't work on vglm()).
# Get the residuals and degrees of freedom
model_residuals <- residuals(scmdlzt, type = "pearson")
df_residuals <- df.residual(scmdlzt)
# Calculate the residual deviance and compare with degrees of freedom
residual_deviance <- sum(model_residuals^2)
p_value <- 1 - pchisq(residual_deviance, df_residuals)
# Print the results
p_value # Yes, indicates over-dispersion

# Clean R enviro first - delete anything unnecessary and clean garbage.
gc()

summary(scmdlzt)
senest <- coef(scmdlzt)
exp(senest) # Under 1, thus:
1/exp(senest)


# 5. Additional check - number of sentences per device type:

corpus$sentences[is.na(corpus$sentences)] <- 0
sentencesnb <- glm.nb(sentences ~ Device , data=corpus)
# Check that indeed over-dispersed:
odTest(sentencesnb)
summary(sentencesnb)
1/exp(coef(sentencesnb))

corpus %>% group_by(Device) %>% 
  summarise(m = mean(sentences))

# Factorise for graph
corpus$sentences_factor <- ifelse(corpus$sentences >= 3, "3+", corpus$sentences)
corpus$sentences_factor <- as.factor(corpus$sentences_factor)
prop.table(table(corpus$sentences_factor, corpus$Device), margin = 2) * 100


ggplot(corpus, aes(x=Device, fill = sentences_factor)) + 
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="All-uppercase messages (%)", x = "", fill ="Uppercase") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)



# WORD LENGTH

# 1. Word length in characters

tidy_corpus$character_count <- nchar(tidy_corpus$word)

# 2. Descriptive statistics

group_by(tidy_corpus, Device) %>%
  summarise(
    mean = mean(character_count),
    sd = sd(character_count),
    median = median(character_count, na.rm = TRUE)
  )
# Boxplot
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
# Histogram
ggplot(tidy_corpus, aes(character_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of word tokens per sentence",
       y = "Count") 

# 3. Inferential statistics
# Take sample as too large otherwise.
sample_words <- tidy_corpus %>% filter(character_count > 0) %>%  group_by(Device) %>% slice_sample(n = 500000)
ccmdlzt <- vglm(character_count ~ Device, family = posnegbinomial(), data = sample_words)

# Double-check overdispersion (odTest() doesn't work on vglm()).
# Get the residuals and degrees of freedom
model_residuals <- residuals(ccmdlzt, type = "pearson")
df_residuals <- df.residual(ccmdlzt)
residual_deviance <- sum(model_residuals^2)
p_value <- 1 - pchisq(residual_deviance, df_residuals)
# Print the results
p_value # Yes, indicates over-dispersion


# Clean R enviro first - delete anything unnecessary and clean garbage.
gc()

summary(ccmdlzt)
wordest <- coef(ccmdlzt)
exp(wordest)
