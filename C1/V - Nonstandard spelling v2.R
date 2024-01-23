# V. Non-standard spelling
library(tidyverse)
library(tidytext)
library(stringi)
library(emo)
library(AER)
library(MASS)
library(pscl)
library(ggthemes)
library(RColorBrewer)


setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/LTC")
corpus <- read_csv2("twitter_big_corpus_clean.csv")

# All-caps words, 3+

# Clean it once again for all-caps. This time, without lowercasing it

corpus$content_clean <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean <- str_remove_all(corpus$content_clean, "#[\\S]+")
corpus$content_clean <- str_remove_all(corpus$content_clean, "@[A-z0-9_]+")
corpus$content_clean <- ji_replace_all(corpus$content_clean, "!")
# get emoticons from III - Emojis.R
corpus$content_clean <- str_replace_all(corpus$content_clean, all_emoticon, "!")

corpus$allcaps <- str_extract_all(corpus$content_clean, "\\b[A-Z]{3,}\\b")
corpus$allcaps_count <- str_count(corpus$content_clean, "\\b[A-Z]{3,}\\b")


allcap_corp <- corpus %>%
  group_by(Device) %>%
  summarise(allcap_freq = sum(allcaps_count))
allcap_corp$freq_normalised <- ifelse(allcap_corp$Device == "Phone", allcap_corp$allcap_freq * 1000 / 10897628, 
                                  allcap_corp$allcap_freq * 1000 / 3531794)  

# ONE word capitalised only
onecap_corp <- corpus %>% 
  subset(allcaps_count == 1)
onecap_corp_red <- onecap_corp %>%
  group_by(Device) %>%
  summarise(allcap_freq = sum(allcaps_count))
onecap_corp_red$freq_normalised <- ifelse(onecap_corp_red$Device == "Phone", onecap_corp_red$allcap_freq * 1000 / 10897628, 
                                      onecap_corp_red$allcap_freq * 1000 / 3531794) 

# PLOT
ggplot(allcap_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="All-cap words (3+)",
       y = "Frequency (per 1.000.000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# Stats

corp0 <- corpus %>% subset(word_count > 0)

acmdlnb <- glm.nb(allcaps_count ~ Device + offset(log(word_count)), data=corp0)
summary(acmdlnb)

# Check that indeed over-dispersed:
odTest(acmdlnb)
acmdlnb <- cbind(Estimate = coef(acmdlnb), confint(acmdlnb))
exp(acmdlnb)

allcap_corp


corpus_ac <- unnest(corpus, allcaps)
ac_table <- corpus_ac %>%
  dplyr::select(c(Device, allcaps)) %>%
  group_by(Device, allcaps)
ac_table$ac_token<- gsub('([[:alpha:]])\\1+', '\\1', ac_table$allcaps)

ac_table2 <- ac_table %>%
  group_by(Device, ac_token) %>%
  summarise(ac_type_frequency = n()) %>%
  top_n(10)
ac_table2$normalised_freq <- ifelse(ac_table2$Device == "Phone", ac_table2$ac_type_frequency * 1000 / 10897628  , 
                                          ac_table2$ac_type_frequency * 1000 / 3531794)
ac_table2$normalised_freq <- round(ac_table2$normalised_freq)
ac_table2 <- ac_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Chap 5 - Microlinguistic features v1")
write_csv2(ac_table2, "10_frequently_allcaps.csv")


# All-caps comments, all thing

corpus$allcapscomment_count <- str_detect(corpus$content_clean, "[a-z]", negate = TRUE)

a <- table(corpus$allcapscomment_count, corpus$Device)
b <- prop.table(a, margin=2)
b

# PLOT
ggplot(corpus, aes(x=Device, fill = allcapscomment_count)) + 
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

accmdl <- glm(allcapscomment_count ~ Device, data = corpus, family = "binomial")
summary(accmdl)
exp(cbind(OR = coef(accmdl), confint(accmdl)))

corpus %>%
  group_by(Device, allcapscomment_count) %>%
  summarise(capped = n()) %>%
  mutate(total = sum(capped),
         percent = capped/total)

# All-lowercase comments, all thing

corpus <- corpus %>%  subset(str_detect(corpus$content_clean, "[A-z]"))
corpus$alllowcomment_count <- str_detect(corpus$content_clean, "[A-Z]", negate = TRUE)

a <- table(corpus$alllowcomment_count, corpus$Device)
b <- prop.table(a, margin=2)
b

# PLOT
ggplot(corpus, aes(x=Device, fill = alllowcomment_count)) + 
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="All-lowercase s (%)", x = "", fill ="Lowercase") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)

alcmdl <- glm(alllowcomment_count ~ Device, data = corpus, family = "binomial")
summary(alcmdl)
exp(cbind(OR = coef(alcmdl), confint(alcmdl)))

corpus %>%
  group_by(Device, alllowcomment_count) %>%
  summarise(lowed = n()) %>%
  mutate(total = sum(lowed),
         percent = lowed/total)


# DID THIS ONE!!!
corpus$allcaps_count <- str_count(corpus$content_clean, "[a-z]+\\W+\\b[A-Z]{2,}\\b")
#corpus$allcaps_count <- str_count(corpus$content_clean, "([^[A-Z]]+)\\W*\\b[A-Z]{3,}\\b\\W*([^[A-Z]]+|$)")



allcap_corp <- corpus %>%
  group_by(Device) %>%
  summarise(allcap_freq = sum(allcaps_count))


allcap_corp$freq_normalised <- ifelse(allcap_corp$Device == "Phone", allcap_corp$allcap_freq * 1000 / 10897628, 
                                      allcap_corp$allcap_freq * 1000 / 3531794)  

allcap_corp

acmdlnb <- glm.nb(allcap_count ~ Device + offset(log(word_count)), data=corpus)
summary(acmdlnb)

# Check that indeed over-dispersed:
odTest(acmdlnb)
acmdlnb <- cbind(Estimate = coef(acmdlnb), confint(acmdlnb))
exp(acmdlnb)

allcap_corp


# PLOT
ggplot(allcap_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="All-uppercase words",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# Stats

# Delete extra letters
oc_table$capitalised_word<- gsub('([[:alpha:]])\\1+', '\\1', oc_table$capitalised_word)
oc_table


oc_table2 <- oc_table %>%
  group_by(Device, capitalised_word) %>%
  summarise(oc_type_frequency = n()) %>%
  top_n(50)
oc_table2$normalised_freq <- ifelse(oc_table2$Device == "Phone", oc_table2$oc_type_frequency * 1000 / 10897628  , 
                                    oc_table2$oc_type_frequency * 1000 / 3531794)
oc_table2$normalised_freq <- round(oc_table2$normalised_freq)
oc_table2 <- oc_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
oc_table2

write_csv2(oc_table2, "top_50_single_caps.csv")


oc_corpus <- corpus %>% subset(allcapscomment == FALSE)
ggplot(oc_corpus, aes(x = Device, fill = one_cap)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="Contains single capitalised word (4+ letters)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)

# First letter


firstcapitalletter <- function(x){
  fcl <- str_detect(x, "^[A-Z][^[A-Z]]")
  return(fcl)
}


firstlowercase <- function(x){
  flc <- str_detect(x, "^[a-z]")
  return(flc)
}


clean_urls <- function(x){
  a <- str_replace(x, "https", "URL")
  return(a)
}

corpus$Content <- clean_urls(corpus$Content)
corpus$Capital = apply(corpus[,4], MARGIN = 1, FUN = firstcapitalletter)
corpus$Lowercase = apply(corpus[,4], MARGIN = 1, FUN = firstlowercase)


corpus$Firstletter <- ifelse(corpus$Capital == TRUE, "Uppercase", 
                             ifelse(corpus$Lowercase == TRUE, "Lowercase", NA))

corpus_backup <- corpus

corpus <- subset(corpus, !is.na(Firstletter))
corpus$Firstletter <- factor(corpus$Firstletter, levels = c("Uppercase", "Lowercase"))
#subcorpus <- subset(corpus, !is.na(Firstletter))

ggplot(corpus, aes(x = Device, fill = Firstletter)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="Capitalisation of first letter (%)", x = "", fill ="First Letter") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)

c <-table(corpus$Firstletter, corpus$Device)
d <- prop.table(c, margin = 2)
d

corpus$Firstletter <- as.factor(corpus$Firstletter)
flcmdl <- glm(Firstletter ~ Device, data = corpus, family = "binomial")
summary(flcmdl)
exp(cbind(OR = coef(flcmdl), confint(flcmdl)))

corpus %>%
  group_by(Device, Firstletter) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)


# Letter repetition

corpus$letter_repeat <- str_extract_all(corpus$content_clean, "\\b(?=\\w*([a-z])\\1\\1)\\w+\\b") # At least three
corpus$letter_repeat_count <- str_count(corpus$content_clean, "([a-z])\\1{2,}")
corpus$letter_repeat_count <- str_count(corpus$content_clean, "\\b(?=\\w*([a-z])\\1\\1)\\w+\\b")

#Check is.na rows:
new_DF <- corpus[is.na(corpus$letter_repeat_count),]

lr_corp <- corpus %>% subset(word_count > 0)
lr_corp <- lr_corp %>%
  group_by(Device) %>%
  summarise(lr_freq = sum(letter_repeat_count))
lr_corp$freq_normalised <- ifelse(lr_corp$Device == "Phone", lr_corp$lr_freq * 1000 / 10897628, 
                                      lr_corp$lr_freq * 1000 / 3531794)  
lr_corp

# PLOT
ggplot(lr_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Letter iteration (3+) words",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")


# Stats

lrmdlnb <- glm.nb(letter_repeat_count ~ Device + offset(log(word_count)), data=corpus)
summary(lrmdlnb)

# Check that indeed over-dispersed:
odTest(lrmdlnb)
lrmdlnb <- cbind(Estimate = coef(lrmdlnb), confint(lrmdlnb))
exp(lrmdlnb)

lr_corp

# Are the top 10 the same?

corpus_lr <- unnest(corpus, letter_repeat)
lr_table <- corpus_lr %>%
  dplyr::select(c(Device, letter_repeat)) %>%
  group_by(Device, letter_repeat)

# Delete extra letters
lr_table$lr_token<- gsub('([[:alpha:]])\\1+', '\\1', lr_table$letter_repeat)
lr_table


lr_table2 <- lr_table %>%
  group_by(Device, lr_token) %>%
  summarise(lr_type_frequency = n()) %>%
  top_n(15)
lr_table2$normalised_freq <- ifelse(lr_table2$Device == "Phone", lr_table2$lr_type_frequency * 100000 / 10897628, 
                                          lr_table2$lr_type_frequency * 100000 / 3531794)
lr_table2$normalised_freq <- round(lr_table2$normalised_freq)
lr_table2 <- lr_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Graph - Chap 5 - Microlinguistic features v1")
write_csv2(lr_table2, "10_frequently_letter_rep.csv")


# CAMEL CASE:

cc1 <- "\\b[A-Z]+[a-z]+[A-Z]+[a-z]+[A-Z]+[A-z]*\\b"
cc2 <- "\\b[a-z]+[A-Z]+[a-z]+[A-Z]+[A-z]*\\b"
cc_all <- paste0(c(cc1,cc2), collapse="|")
corpus$cc_token <- str_extract_all(corpus$content_clean, cc_all)
corpus$cc_count <- str_count(corpus$content_clean, cc_all)

cc_corp <- corpus %>% subset(cc_count > 0)
cc_corp <- cc_corp %>%
  group_by(Device) %>%
  summarise(cc_freq = sum(cc_count))
cc_corp$freq_normalised <- ifelse(cc_corp$Device == "Phone", cc_corp$cc_freq * 1000 / 10897628, 
                                  cc_corp$cc_freq * 1000 / 3531794)  
cc_corp
# PLOT


ggplot(cc_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Camel-case words (4/5+ letters)",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# Stats
corpus$cc <- corpus$cc_count > 0
corpus %>%
  group_by(Device, cc) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)

# Stats
corpus$cc_count[is.na(corpus$cc_count)] <- 0
ccmdlnb <- glm.nb(cc_count ~ Device + offset(log(word_count)), data=corpus)
summary(ccmdlnb)

# Check that indeed over-dispersed:
odTest(ccmdlnb)
ccmdlnb_coef <- cbind(Estimate = coef(ccmdlnb), confint(ccmdlnb))
exp(ccmdlnb_coef)

cc_corp
ccmdl <- glm(cc ~ Device, data = corpus, family = "binomial")
summary(ccmdl)

# Are the top 10 the same?

corpus_cc <- unnest(corpus, cc_token)
cc_table <- corpus_cc %>%
  dplyr::select(c(Device, cc_token)) %>%
  group_by(Device, cc_token)


cc_table2 <- cc_table %>%
  group_by(Device, cc_token) %>%
  summarise(cc_type_frequency = n()) %>%
  top_n(10)
cc_table2$normalised_freq <- ifelse(cc_table2$Device == "Phone", cc_table2$cc_type_frequency * 1000 / 10897628 , 
                                    cc_table2$cc_type_frequency * 1000 / 3531794)
cc_table2$normalised_freq <- round(cc_table2$normalised_freq)
cc_table2 <- cc_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Chap 5 - Microlinguistic features v1")
write_csv2(cc_table2, "10_frequently_letter_rep.csv")


# <I> vs <i>

corpus$cap_i <- str_count(corpus$content_clean, "\\bI\\b")
corpus$lower_i <- str_count(corpus$content_clean,"\\bi\\b")
corpus$all_i <- corpus$cap_i + corpus$lower_i

sum(corpus$cap_i) / sum(corpus$all_i)
i_corp <- corpus %>%
  group_by(Device) %>%
  summarise(i_prop = sum(cap_i) / sum(all_i))
i_corp
