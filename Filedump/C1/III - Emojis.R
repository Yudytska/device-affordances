# III. Pictograms
library(tidyverse)
library(tidytext)
library(stringi)
library(emo)
library(AER)
library(MASS)
library(pscl)
library(boot)
library(ggthemes)
library(RColorBrewer)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/LTC")

corpus <- read_csv2("twitter_big_corpus_clean.csv")

corpus$content_clean <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean <- str_remove_all(corpus$content_clean, "#[\\S]+")
corpus$content_clean <- str_remove_all(corpus$content_clean, "@[A-z0-9_]+")


# 1. Emojis

corpus$emoji_count <- ji_count(corpus$Content)
corpus$emoji_token <- ji_extract_all(corpus$Content)

# FREQUENCY
em_corp <- corpus %>%
  group_by(Device) %>%
  summarise(emoji_freq = sum(emoji_count))
em_corp$freq_normalised <- ifelse(em_corp$Device == "Phone", em_corp$emoji_freq * 1000 / 10897628, 
                                   em_corp$emoji_freq * 1000 / 3531794)  

# PLOT
ggplot(em_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Emoji",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
        theme(legend.position="none")

# Are the top 10 used emojis the same?
corpus_ascii <- unnest(corpus, emoji_token)
emoji_table <- corpus_ascii %>%
  dplyr::select(c(Device, emoji_token)) %>%
  group_by(Device, emoji_token)

emoji_table2 <- emoji_table %>%
  group_by(Device, emoji_token) %>%
  summarise(emoji_type_frequency = n()) %>%
  top_n(20)
emoji_table2$normalised_freq <- ifelse(emoji_table2$Device == "Phone", emoji_table2$emoji_type_frequency * 100000 / 10897628, 
                                      emoji_table2$emoji_type_frequency * 100000 / 3531794)
emoji_table2 <- emoji_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
emoji_table2$normalised_freq <- round(emoji_table2$normalised_freq)
View(emoji_table2)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Graph - Chap 5 - Microlinguistic features v1")
write_csv2(emoji_table2, "10_frequently_emojis_v2.csv")


# Multiple emojis?
emoji <- corpus %>% subset(emoji_count > 0)
# descriptive stats:
group_by(emoji, Device) %>%
  summarise(
    count = n(),
    mean = mean(emoji_count, na.rm = TRUE),
    sd = sd(emoji_count, na.rm = TRUE)
  )


# Find how many emoji per 

corpus_em_t <- unnest(corpus, emoji_token)

corpus_em_t2 <- corpus_em_t %>% group_by(Device) %>% slice_sample(n = 5000)


emoji_table_t1 <- corpus_em_t2 %>%
  dplyr::select(c(Device, emoji_token)) %>%
  group_by(Device, emoji_token)


emoji_table2_t1 <- emoji_table_t1 %>%
  summarise(emoji_type_frequency = n()) 
emoji_table2_t1$Device <- as.factor(emoji_table2_t1$Device)
summary(emoji_table2_t1)



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

emoticon <- paste0(c(emoticon1, emoticon2, emoticon3, emoticon4, emoticon5), collapse = "|")

corpus$emoticon_token <- str_extract_all(corpus$content_clean, emoticon)
corpus$emoticon_count <- str_count(corpus$content_clean, emoticon)

et_corp <- corpus %>%
  group_by(Device) %>%
  summarise(emoticon_freq = sum(emoticon_count))
et_corp$freq_normalised <- ifelse(et_corp$Device == "Phone", et_corp$emoticon_freq * 1000 / 10897628, 
                                  et_corp$emoticon_freq * 1000 / 3531794)  


# Emoticon variation
heart <- emoticon_table %>% filter(str_detect(emoticon_token, "&lt;3"))
heart$threes <- str_count(heart$emoticon_token, "3")
prop.table(table(heart$threes, heart$Device), margin = 2)

smile <- emoticon_table %>% filter(str_detect(emoticon_token, "^:\\("))
smile$mouth <- str_count(smile$emoticon_token, "\\(")
prop.table(table(smile$mouth, smile$Device), margin = 2)

# PLOT
ggplot(et_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Emoticons",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# Are the top 10 used emoticons the same?
corpus_et <- unnest(corpus, emoticon_token)
emoticon_table <- corpus_et %>%
  dplyr::select(c(Device, emoticon_token)) %>%
  group_by(Device, emoticon_token)

emoticon_table$emoticon_token <- gsub("([)(|o3dp0)/_])\\1{1,}", "\\1", emoticon_table$emoticon_token)

emoticon_table2 <- emoticon_table %>%
  group_by(Device, emoticon_token) %>%
  summarise(emoticon_type_frequency = n()) %>%
  top_n(20)
emoticon_table2$normalised_freq <- ifelse(emoticon_table2$Device == "Phone", emoticon_table2$emoticon_type_frequency * 1000 / 10897628  , 
                                       emoticon_table2$emoticon_type_frequency * 1000 / 3531794)
emoticon_table2$normalised_freq <- round(emoticon_table2$normalised_freq)
emoticon_table2 <- emoticon_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
emoticon_table2
View(emoticon_table2)
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Graph - Chap 5 - Microlinguistic features v1")
write_csv2(emoticon_table2, "10_frequently_emoticons_v2.csv")

# Multiple emoticons?
emoticon_corp <- corpus %>% subset(emoticon_count > 0)
# descriptive stats:
group_by(emoticon_corp, Device) %>%
  summarise(
    count = n(),
    mean = mean(emoticon_count, na.rm = TRUE),
    sd = sd(emoticon_count, na.rm = TRUE)
  )


# Statistics attempt 2:

corpus <- corpus %>% subset(word_count >0)

# EMOTICONS

group_by(corpus, Device) %>%
  summarise(
    emsum = sum(emoji_count),
    etsum = sum(emoticon_count),
    wcsum = sum(word_count),
    normem = emsum * 1000 / wcsum,
    normet = etsum * 1000 / wcsum
  )

emmdlnb <- glm.nb(emoji_count ~ Device + offset(log(word_count)), data=corpus)
summary(emmdlnb)

# Check that indeed over-dispersed:
odTest(emmdlnb)
emest <- cbind(Estimate = coef(emmdlnb), confint(emmdlnb))
exp(emest)


# EMOTICONS
etmdlnb <- glm.nb(emoticon_count ~ Device + offset(log(word_count)), data=corpus)
summary(etmdlnb)

# Check that indeed over-dispersed:
odTest(etmdlnb)
etest <- cbind(Estimate = coef(etmdlnb), confint(etmdlnb))
exp(etest)
