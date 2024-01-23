# CAPITALISATION!
library(tidyverse)
library(tidytext)
library(emo)

# All-caps words, 3+

# Clean it once again for all-caps. This time, without lowercasing it

corpus$content_clean <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean <- str_remove_all(corpus$content_clean, "#[\\S]+")
corpus$content_clean <- str_remove_all(corpus$content_clean, "@[A-z0-9_]+")
corpus$content_clean <- ji_replace_all(corpus$content_clean, "!")
# get emoticons from III - Emojis.R
corpus$content_clean <- str_replace_all(corpus$content_clean, emoticon, "!")

corpus$content_clean2 <- corpus$content_clean
corpus$content_clean <- str_replace_all(corpus$content_clean, "\\b[A-Z]*[a-z]+[A-Z]*[a-z]*\\b", "lrcase")

# This too?
corpus$content_clean<- gsub('([[:alpha:]])\\1+', '\\1', corpus$content_clean)

# Bigrams
bigrams <- corpus %>%
  unnest_tokens(bigram, content_clean, token = "ngrams", n = 2)
bigrams <- bigrams %>% 
  filter(str_detect(bigram, "lrcase", negate = TRUE)) %>% 
  filter(str_detect(bigram, "[0-9]", negate = TRUE))

bigrams <- bigrams %>% 
  select(c(Device, Content, bigram))

bigram_count <- bigrams %>%
  group_by(Device, bigram) %>%
  summarise(frequency = n()) %>%
  top_n(20)%>% 
  arrange(desc(frequency)) %>% 
  arrange(Device) %>% 
  toupper(bigram)
View(bigram_count)

bigram_count$bigram <- toupper(bigram_count$bigram)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/GRAPH - Chap 5 - Microlinguistic features v1")
write_csv2(bigram_count, "20_frequently_capped bigrams.csv")


# Try to filter out stopwords
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigram_count <- bigrams_united %>%
  group_by(Device, bigram) %>%
  summarise(frequency = n()) %>%
  top_n(20)%>% 
  arrange(desc(frequency)) %>% 
  arrange(Device)
View(bigram_count)

