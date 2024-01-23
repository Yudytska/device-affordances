# IV. PUNCTUATION

library(tidyverse)
library(emo)
library(MASS)
library(pscl)
library(ggthemes)
library(RColorBrewer)


setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/LTC")
corpus <- read_csv2("twitter_big_corpus_clean.csv")

# Change content_clean

corpus$content_clean <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean <- str_remove_all(corpus$content_clean, "#[\\S]+")
corpus$content_clean <- str_remove_all(corpus$content_clean, "@[A-z0-9_]+")
corpus$content_clean <- ji_replace_all(corpus$content_clean, "")
# get emoticons from III - punctations.R
corpus$content_clean <- str_remove_all(corpus$content_clean, all_emoticon)
# clean '
corpus$content_clean <- str_replace_all(corpus$content_clean, "’", "'")
corpus$content_clean <- str_replace_all(corpus$content_clean, "“", "\"")
corpus$content_clean <- str_replace_all(corpus$content_clean, "”", "\"")


# Does it make more sense to go by character count?
corpus$char_count <- nchar(corpus$content_clean)
group_by(corpus, Device) %>%
  summarise(
    count = n(),
    sum = sum(char_count),
    mean = mean(char_count),
    median = median(char_count),
    sd = sd(char_count)
  )
# Find all punctuation marks?

punct1 <- "[[:punct:]]+"
punct2 <- "([[:punct:]])\\1+"
punct3 <- "[[:punct:]]"

corpus$punctuation_token <- str_extract_all(corpus$content_clean, punct3)
corpus$punctuation_count <- str_count(corpus$content_clean, punct1)

punct_corp <- corpus %>% subset(punctuation_count > 0)
punct_corp <- punct_corp %>%
  group_by(Device) %>%
  dplyr::summarise(punct_freq = sum(punctuation_count))
punct_corp$freq_normalised <- ifelse(punct_corp$Device == "Phone", punct_corp$punct_freq * 1000 / 10897628, 
                                  punct_corp$punct_freq * 1000 / 3531794)  
punct_corp

# PLOT
ggplot(punct_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Punctuation (all)",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# Are the top 10 used punctuation the same?

corpus_punct <- unnest(corpus, punctuation_token)
punctuation_table <- corpus_punct %>%
  dplyr::select(-c(ID, Content,word_count)) %>%
  group_by(Device, punctuation_token)

punctuation_table2 <- punctuation_table %>%
  group_by(Device, punctuation_token) %>%
  summarise(punctuation_type_frequency = n()) %>%
  top_n(15)
punctuation_table2$normalised_freq <- ifelse(punctuation_table2$Device == "Phone", punctuation_table2$punctuation_type_frequency * 1000 / 10897628  , 
                                          punctuation_table2$punctuation_type_frequency * 1000 / 3531794)
punctuation_table2$normalised_freq <- round(punctuation_table2$normalised_freq)
punctuation_table2 <- punctuation_table2 %>% 
  arrange(desc(normalised_freq)) %>% 
  arrange(Device)

View(punctuation_table2)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Graph - Chap 5 - Microlinguistic features v1")
write_csv2(punctuation_table2, "10_frequently_punctuation_just_one_mark.csv")

punct_corp <- corpus %>% subset(punctuation_count > 0)
punct_corp <- punct_corp %>%
  group_by(Device) %>%
  summarise(punct_freq = sum(punctuation_count))
punct_corp$freq_normalised <- ifelse(punct_corp$Device == "Phone", punct_corp$punct_freq * 1000 / 10897628, 
                                  punct_corp$punct_freq * 1000 / 3531794)  
punct_corp

# Statistics attempt 2:

corpus <- corpus %>% subset(word_count >0)

# punctuation

group_by(corpus, Device) %>%
  summarise(
    emsum = sum(punctation_count),
    punctsum = sum(punctuation_count),
    wcsum = sum(word_count),
    normem = emsum * 1000000 / wcsum,
    normpunct = punctsum * 1000000 / wcsum
  )

punctmdlnb <- glm.nb(punctuation_count ~ Device + offset(log(word_count)), data=corpus)
summary(punctmdlnb)

# Check that indeed over-dispersed:
odTest(punctmdlnb)
punctest <- cbind(Estimate = coef(punctmdlnb), confint(punctmdlnb))
exp(punctest)



# APOSTROPHE
corpus$apostrophe_token <- str_extract_all(corpus$content_clean, "'")
corpus$apostrophe_count <- str_count(corpus$content_clean, "\\w+['’]\\w+")
corpus$apostrophe_count <- str_count(corpus$content_clean, "'")

apostrophe_corp <- corpus %>% subset(apostrophe_count > 0)
apostrophe_corp <- apostrophe_corp %>%
  group_by(Device) %>%
  summarise(apostrophe_freq = sum(apostrophe_count))
apostrophe_corp$freq_normalised <- ifelse(apostrophe_corp$Device == "Phone", apostrophe_corp$apostrophe_freq * 1000 / 10897628, 
                                          apostrophe_corp$apostrophe_freq * 1000 / 3531794)  
apostrophe_corp


# PLOT
ggplot(apostrophe_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Apostrophe",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")


# Statistics attempt 2:

apostrophemdlnb <- glm.nb(apostrophe_count ~ Device + offset(log(word_count)), data=corpus)
summary(apostrophemdlnb)

# Check that indeed over-dispersed:
odTest(apostrophemdlnb)
apostropheest <- cbind(Estimate = coef(apostrophemdlnb), confint(apostrophemdlnb))
exp(apostropheest)


# ELLIPSIS

corpus$ellipsis_token <- str_extract_all(corpus$content_clean, "(\\.\\.\\.)\\.+")
corpus$ellipsis_count <- str_count(corpus$content_clean, "(\\.\\.\\.)\\.+")


ellipsis_corp <- corpus %>% subset(ellipsis_count > 0)
ellipsis_corp <- ellipsis_corp %>%
  group_by(Device) %>%
  summarise(ellipsis_freq = sum(ellipsis_count))
ellipsis_corp$freq_normalised <- ifelse(ellipsis_corp$Device == "Phone", ellipsis_corp$ellipsis_freq * 1000 / 10897628, 
                                        ellipsis_corp$ellipsis_freq * 1000 / 3531794)  
ellipsis_corp

# Statistics attempt 2:

ellipsismdlnb <- glm.nb(ellipsis_count ~ Device + offset(log(word_count)), data=corpus)
summary(ellipsismdlnb)

# Check that indeed over-dispersed:
odTest(ellipsismdlnb)
ellipsisest <- cbind(Estimate = coef(ellipsismdlnb), confint(ellipsismdlnb))
exp(ellipsisest)


# PLOT
ggplot(apostrophe_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Ellipsis",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# PERIOD

corpus$period_token <- str_extract_all(corpus$content_clean, "(?<!\\.)\\.(?!\\.+)")
corpus$period_count <- str_count(corpus$content_clean, "(?<!\\.)\\.(?!\\.+)")
corpus$Period <- corpus$period_count

period_corp <- corpus %>% subset(period_count > 0)
period_corp <- period_corp %>%
  group_by(Device) %>%
  summarise(period_freq = sum(period_count))
period_corp$freq_normalised <- ifelse(period_corp$Device == "Phone", period_corp$period_freq * 1000 / 10897628, 
                                      period_corp$period_freq * 1000 / 3531794)  
period_corp

ggplot(period_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Period",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# Statistics attempt 2:

periodmdlnb <- glm.nb(period_count ~ Device + offset(log(word_count)), data=corpus)
summary(periodmdlnb)

# Check that indeed over-dispersed:
odTest(periodmdlnb)
periodest <- cbind(Estimate = coef(periodmdlnb), confint(periodmdlnb))
exp(periodest)


# PERIOD-FINAL:

corpus$period_final <- str_detect(corpus$content_clean, "(?<!\\.)\\.(?!\\.+)$")
subcorpus <- subset(corpus, !is.na(period_final))
ggplot(corpus, aes(x = Device, fill = period_final)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="Message ends with period (%)", x = "", fill ="Period-final") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)

c <-table(corpus$period_final, corpus$Device)
d <- prop.table(c, margin = 2)
d

corpus$period_final <- as.factor(corpus$period_final)
flcmdl <- glm(period_final ~ Device, data = corpus, family = "binomial")
summary(flcmdl)
exp(cbind(OR = coef(flcmdl), confint(flcmdl)))

subcorpus %>%
  group_by(Device, period_final) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)


# COMMA

corpus$comma_token <- str_extract_all(corpus$content_clean, ",")
corpus$comma_count <- str_count(corpus$content_clean, "(?<!,),(?!,+)")
corpus$Comma <- corpus$comma_count

comma_corp <- corpus %>% subset(comma_count > 0)
comma_corp <- comma_corp %>%
  group_by(Device) %>%
  summarise(comma_freq = sum(comma_count))
comma_corp$freq_normalised <- ifelse(comma_corp$Device == "Phone", comma_corp$comma_freq * 1000 / 10897628, 
                                     comma_corp$comma_freq * 1000 / 3531794)  
comma_corp

# Statistics attempt 2:

commamdlnb <- glm.nb(comma_count ~ Device + offset(log(word_count)), data=corpus)
summary(commamdlnb)

# Check that indeed over-dispersed:
odTest(commamdlnb)
commaest <- cbind(Estimate = coef(commamdlnb), confint(commamdlnb))
exp(commaest)


# PLOT
ggplot(comma_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Comma",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

# OTHER SYNTAGMATIC SIGNS

synta <- "(\\-)|(\\;)|(\\:)|(\\-)|(\\-)"

corpus$synta_token <- str_extract_all(corpus$content_clean, synta)
corpus$synta_count <- str_count(corpus$content_clean, synta)

synta_corp <- corpus %>% subset(synta_count > 0)
synta_corp <- synta_corp %>%
  group_by(Device) %>%
  summarise(synta_freq = sum(synta_count))
synta_corp$freq_normalised <- ifelse(synta_corp$Device == "Phone", synta_corp$synta_freq * 1000 / 10897628, 
                                     synta_corp$synta_freq * 1000 / 3531794)  
synta_corp

# PLOT
ggplot(synta_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Other syntactic signs",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")


# Statistics attempt 2:

syntamdlnb <- glm.nb(synta_count ~ Device + offset(log(word_count)), data=corpus)
summary(syntamdlnb)

# Check that indeed over-dispersed:
odTest(syntamdlnb)
syntaest <- cbind(Estimate = coef(syntamdlnb), confint(syntamdlnb))
exp(syntaest)



# Quotation marks

corpus$quotation_count <- str_count(corpus$content_clean, "[\\s\\b^][\"'«»“”’].+[\"'«»“”’][\\s\\b$]")

quotation_corp <- corpus %>% subset(quotation_count > 0)
quotation_corp <- quotation_corp %>%
  group_by(Device) %>%
  summarise(quotation_freq = sum(quotation_count))
quotation_corp$freq_normalised <- ifelse(quotation_corp$Device == "Phone", quotation_corp$quotation_freq * 1000 / 10897628, 
                                         quotation_corp$quotation_freq * 1000 / 3531794)  
quotation_corp

# Statistics attempt 2:

quotationmdlnb <- glm.nb(quotation_count ~ Device + offset(log(word_count)), data=corpus)
summary(quotationmdlnb)

# Check that indeed over-dispersed:
odTest(quotationmdlnb)
quotationest <- cbind(Estimate = coef(quotationmdlnb), confint(quotationmdlnb))
exp(quotationest)


# PLOT
ggplot(quotation_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Quotation marks",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")






# EXCLAMATION MARKS

corpus$exclamation_token <- str_extract_all(corpus$content_clean, "\\!")
corpus$exclamation_count <- str_count(corpus$content_clean, "\\!+")


exclamation_corp <- corpus %>% subset(exclamation_count > 0)
exclamation_corp <- exclamation_corp %>%
  group_by(Device) %>%
  summarise(exclamation_freq = sum(exclamation_count))
exclamation_corp$freq_normalised <- ifelse(exclamation_corp$Device == "Phone", exclamation_corp$exclamation_freq * 1000 / 10897628, 
                                           exclamation_corp$exclamation_freq * 1000 / 3531794)  
exclamation_corp


# Statistics attempt:

corpus <- corpus %>% subset(word_count> 0)

exclamationmdlnb <- glm.nb(exclamation_count ~ Device + offset(log(word_count)), data=corpus)

summary(exclamationmdlnb)

group_by(corpus, Device) %>%
  summarise(
    sum = sum(exclamation_count),
    mean = mean(exclamation_count),
    median = median(exclamation_count),
    sd = sd(exclamation_count), 
    max = max(exclamation_count)
  )

# Check that indeed over-dispersed:
odTest(exclamationmdlnb)
exclamationest <- cbind(Estimate = coef(exclamationmdlnb), confint(exclamationmdlnb))
exp(exclamationest)


excl_corpus <- corpus %>% subset(exclamation_count > 0)
group_by(excl_corpus, Device) %>%
  summarise(
    sum = sum(exclamation_count),
    mean = mean(exclamation_count),
    median = median(exclamation_count),
    sd = sd(exclamation_count),
    max = max(exclamation_count)
  )

excl_corpus20 <- corpus %>% subset(exclamation_count > 20)
group_by(excl_corpus20, Device) %>%
  summarise(
    sum = sum(exclamation_count),
    mean = mean(exclamation_count),
    median = median(exclamation_count),
    sd = sd(exclamation_count),
    max = max(exclamation_count)
  )



# Attempt 2???

corpus$has_exclamation <- str_detect(corpus$content_clean, "!")
subcorpus <- subset(corpus, !is.na(has_exclamation))
ggplot(subcorpus, aes(x = Device, fill = has_exclamation)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="Comments contain 1+ exclamation marks (%)", x = "", fill ="Has exclamation mark") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)

c <-table(subcorpus$has_exclamation, subcorpus$Device)
d <- prop.table(c, margin = 2)
d

subcorpus$has_exclamation <- as.factor(subcorpus$has_exclamation)
flcmdl <- glm(has_exclamation ~ Device, data = subcorpus, family = "binomial")
summary(flcmdl)
exp(cbind(OR = coef(flcmdl), confint(flcmdl)))

subcorpus %>%
  group_by(Device, has_exclamation) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)

# GROUPS


corpus$excl_count <- str_count(corpus$content_clean, "!!+")


excl_corp <- corpus %>% subset(excl_count > 0)
excl_corp <- excl_corp %>%
  group_by(Device) %>%
  summarise(excl_freq = sum(excl_count))
excl_corp$freq_normalised <- ifelse(excl_corp$Device == "Phone", excl_corp$excl_freq * 1000 / 10897628, 
                                    excl_corp$excl_freq * 1000 / 3531794)  
excl_corp

# Statistics attempt:

exclmdlnb <- glm.nb(excl_count ~ Device + offset(log(word_count)), data=corpus)
summary(exclmdlnb)

# Check that indeed over-dispersed:
odTest(exclmdlnb)
exclest <- cbind(Estimate = coef(exclmdlnb), confint(exclmdlnb))
exp(exclest)


ggplot(excl_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Multiple exclamation marks <!!+>",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")



# GROUPS CONTINUED: WHAT IS AVERAGE OF GROUP?

corpus$excl_ellipsis_token <- str_extract_all(corpus$content_clean, "(\\!\\!)\\!*")
corpus$excl_ellipsis_count <- str_count(corpus$content_clean, "(\\!\\!)\\!*")



excl_ellipsis_corp <- corpus %>% subset(excl_ellipsis_count > 0)
excl_ellipsis_corp <- excl_ellipsis_corp %>%
  group_by(Device) %>%
  summarise(excl_ellipsis_freq = sum(excl_ellipsis_count))
excl_ellipsis_corp$freq_normalised <- ifelse(excl_ellipsis_corp$Device == "Phone", excl_ellipsis_corp$excl_ellipsis_freq * 1000 / 10897628, 
                                             excl_ellipsis_corp$excl_ellipsis_freq * 1000 / 3531794)  
excl_ellipsis_corp

# Statistics attempt 2:
corpus2 <- corpus %>% subset(word_count > 0)
excl_ellipsismdlnb <- glm.nb(excl_ellipsis_count ~ Device + offset(log(word_count)), data=corpus2)
summary(excl_ellipsismdlnb)

# Check that indeed over-dispersed:
odTest(excl_ellipsismdlnb)
excl_ellipsisest <- cbind(Estimate = coef(excl_ellipsismdlnb), confint(excl_ellipsismdlnb))
exp(excl_ellipsisest)


# How many <!> in there?

excl_ellipsis_corp <- unnest(corpus, excl_ellipsis_token)
excl_ellipsis_corp$excl_count <- str_count(excl_ellipsis_corp$excl_ellipsis_token, "\\!")

group_by(excl_ellipsis_corp, Device) %>%
  summarise(
    sum = sum(excl_count),
    mean = mean(excl_count),
    median = median(excl_count),
    sd = sd(excl_count),
    max = max(excl_count)
  )

excl_ellipsismdlnb2 <- glm.nb(excl_count ~ Device, data=excl_ellipsis_corp)
summary(excl_ellipsismdlnb2)

# Check that indeed over-dispersed:
odTest(excl_ellipsismdlnb2)
excl_ellipsisest <- cbind(Estimate = coef(excl_ellipsismdlnb2), confint(excl_ellipsismdlnb2))
exp(excl_ellipsisest)


p <- ggplot(excl_ellipsis_corp, aes(excl_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of <!> in <!> ellipsis",
       y = "Count") 
p +
  xlim(c(0, 15))


# Single <!> on their own


corpus$single_excl_token <- str_extract_all(corpus$content_clean, "(?<!\\!)\\!(?!\\!+)")
corpus$single_excl_count <- str_count(corpus$content_clean, "(?<!\\!)\\!(?!\\!+)")


single_excl_corp <- corpus %>% subset(single_excl_count > 0)
single_excl_corp <- single_excl_corp %>%
  group_by(Device) %>%
  summarise(single_excl_freq = sum(single_excl_count))
single_excl_corp$freq_normalised <- ifelse(single_excl_corp$Device == "Phone", single_excl_corp$single_excl_freq * 1000 / 10897628, 
                                           single_excl_corp$single_excl_freq * 1000 / 3531794)  
single_excl_corp

# Statistics attempt 2:

single_exclmdlnb <- glm.nb(single_excl_count ~ Device + offset(log(word_count)), data=corpus)
summary(single_exclmdlnb)

# Check that indeed over-dispersed:
odTest(single_exclmdlnb)
single_exclest <- cbind(Estimate = coef(single_exclmdlnb), confint(single_exclmdlnb))
exp(single_exclest)


ggplot(excl_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Multiple exclamation marks <!!+>",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

ggplot(single_excl_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Single exclamation mark",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")


# 5. QUESTION

corpus$question_token <- str_extract_all(corpus$content_clean, "\\?")
corpus$question_count <- str_count(corpus$content_clean, "\\?")


question_corp <- corpus %>% subset(question_count > 0)
question_corp <- question_corp %>%
  group_by(Device) %>%
  summarise(question_freq = sum(question_count))
question_corp$freq_normalised <- ifelse(question_corp$Device == "Phone", question_corp$question_freq * 1000 / 10897628, 
                                        question_corp$question_freq * 1000 / 3531794)  
question_corp

# Statistics attempt 2:

corpus <- corpus %>% subset(word_count > 0)
questionmdlnb <- glm.nb(question_count ~ Device + offset(log(word_count)), data=corpus)
questionmdlnb2 <- glm.nb(question_count ~ Device, data=corpus)
summary(questionmdlnb)
summary(questionmdlnb2)

group_by(corpus, Device) %>%
  summarise(
    sum = sum(question_count),
    mean = mean(question_count),
    median = median(question_count),
    sd = sd(question_count), 
    max = max(question_count)
  )

# Check that indeed over-dispersed:
odTest(questionmdlnb)
questionest <- cbind(Estimate = coef(questionmdlnb), confint(questionmdlnb))
exp(questionest)


# Normalise re characters?
question_corp <- corpus %>% subset(question_count > 0)
group_by(question_corp, Device) %>% 
  summarise(n = sum(char_count))

question_corp <- corpus %>% subset(question_count > 0)
question_corp <- question_corp %>%
  group_by(Device) %>%
  summarise(question_freq = sum(question_count))
question_corp$freq_normalised <- ifelse(question_corp$Device == "Phone", question_corp$question_freq * 1000000 / 5893833, 
                                        question_corp$question_freq * 1000000 / 2333824)  
question_corp

questionmdlnb <- glm.nb(question_count ~ Device + offset(log(char_count)), data=corpus)
summary(questionmdlnb)

# Check that indeed over-dispersed:
odTest(questionmdlnb)
questionest <- cbind(Estimate = coef(questionmdlnb), confint(questionmdlnb))
exp(questionest)




quest_corpus <- corpus %>% subset(question_count > 0)
group_by(quest_corpus, Device) %>%
  summarise(
    sum = sum(question_count),
    mean = mean(question_count),
    median = median(question_count),
    sd = sd(question_count),
    max = max(question_count)
  )

quest_corpus20 <- corpus %>% subset(question_count > 20)
group_by(quest_corpus20, Device) %>%
  summarise(
    sum = sum(question_count),
    mean = mean(question_count),
    median = median(question_count),
    sd = sd(question_count),
    max = max(question_count)
  )

t.test(question_count ~ Device, excl_corpus)

# Attempt 3???

corpus$has_question <- str_detect(corpus$content_clean, "\\?")
subcorpus <- subset(corpus, !is.na(has_question))
ggplot(subcorpus, aes(x = Device, fill = has_question)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") +
  theme_clean() +
  labs(y="Comments contain 1+ question marks (%)", x = "", fill ="Has question mark") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  scale_y_continuous(labels = scales::percent)

c <-table(subcorpus$has_question, subcorpus$Device)
d <- prop.table(c, margin = 2)
d

subcorpus$has_question <- as.factor(subcorpus$has_question)
flcmdl <- glm(has_question ~ Device, data = subcorpus, family = "binomial")
summary(flcmdl)
exp(cbind(OR = coef(flcmdl), confint(flcmdl)))

subcorpus %>%
  group_by(Device, has_question) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percent = n/total)

# GROUPS


corpus$quest_token <- str_extract_all(corpus$content_clean, "(?<!\\?)\\?")
corpus$quest_count <- str_count(corpus$content_clean, "\\?\\?+")


quest_corp <- corpus %>% subset(quest_count > 0)
quest_corp <- quest_corp %>%
  group_by(Device) %>%
  summarise(quest_freq = sum(quest_count))
quest_corp$freq_normalised <- ifelse(quest_corp$Device == "Phone", quest_corp$quest_freq * 1000 / 10897628, 
                                     quest_corp$quest_freq * 1000 / 3531794)  
quest_corp

# Statistics attempt 2:

questmdlnb <- glm.nb(quest_count ~ Device + offset(log(word_count)), data=corpus)
summary(questmdlnb)

# Check that indeed over-dispersed:
odTest(questmdlnb)
questest <- cbind(Estimate = coef(questmdlnb), confint(questmdlnb))
exp(questest)




ggplot(quest_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Multiple question marks <??+>",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")

ggplot(single_quest_corp, aes(y=freq_normalised, x=Device, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="Single question mark",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12)) +
  theme(legend.position="none")




# GROUPS CONTINUED: WHAT IS AVERAGE OF GROUP?

corpus$quest_ellipsis_token <- str_extract_all(corpus$content_clean, "(\\?\\?)\\?*")
corpus$quest_ellipsis_count <- str_count(corpus$content_clean, "(\\?\\?)\\?*")



quest_ellipsis_corp <- corpus %>% subset(quest_ellipsis_count > 0)
quest_ellipsis_corp <- quest_ellipsis_corp %>%
  group_by(Device) %>%
  summarise(quest_ellipsis_freq = sum(quest_ellipsis_count))
quest_ellipsis_corp$freq_normalised <- ifelse(quest_ellipsis_corp$Device == "Phone", quest_ellipsis_corp$quest_ellipsis_freq * 1000 / 10897628, 
                                              quest_ellipsis_corp$quest_ellipsis_freq * 1000 / 3531794)  
quest_ellipsis_corp

# Statistics attempt 2:

quest_ellipsismdlnb <- glm.nb(quest_ellipsis_count ~ Device + offset(log(word_count)), data=corpus2)
summary(quest_ellipsismdlnb)

# Check that indeed over-dispersed:
odTest(quest_ellipsismdlnb)
quest_ellipsisest <- cbind(Estimate = coef(quest_ellipsismdlnb), confint(quest_ellipsismdlnb))
exp(quest_ellipsisest)


# How many <!> in there?

quest_ellipsis_corp <- unnest(corpus, quest_ellipsis_token)
quest_ellipsis_corp$quest_count <- str_count(quest_ellipsis_corp$quest_ellipsis_token, "\\?")

group_by(quest_ellipsis_corp, Device) %>%
  summarise(
    sum = sum(quest_count),
    mean = mean(quest_count),
    median = median(quest_count),
    sd = sd(quest_count),
    max = max(quest_count)
  )

quest_ellipsismdlnb2 <- glm.nb(quest_count ~ Device, data=quest_ellipsis_corp)
summary(quest_ellipsismdlnb2)

# Check that indeed over-dispersed:
odTest(quest_ellipsismdlnb2)
quest_ellipsisest <- cbind(Estimate = coef(quest_ellipsismdlnb2), confint(quest_ellipsismdlnb2))
exp(quest_ellipsisest)


p <- ggplot(quest_ellipsis_corp, aes(quest_count, fill = Device)) + geom_histogram(binwidth = 1) +
  facet_grid(Device ~ ., scales = "free") +
  theme_clean(base_size = 15)+
  scale_fill_brewer(palette = "Paired") +
  labs(x="Number of <!> in <!> ellipsis",
       y = "Count") 
p +
  xlim(c(0, 20))



# Single <!> on their own


corpus$single_quest_token <- str_extract_all(corpus$content_clean, "(?<!\\?)\\?(?!\\?+)")
corpus$single_quest_count <- str_count(corpus$content_clean, "(?<!\\?)\\?(?!\\?+)")


single_quest_corp <- corpus %>% subset(single_quest_count > 0)
single_quest_corp <- single_quest_corp %>%
  group_by(Device) %>%
  summarise(single_quest_freq = sum(single_quest_count))
single_quest_corp$freq_normalised <- ifelse(single_quest_corp$Device == "Phone", single_quest_corp$single_quest_freq * 1000 / 10897628, 
                                            single_quest_corp$single_quest_freq * 1000 / 3531794)  
single_quest_corp

# Statistics attempt 2:

single_questmdlnb <- glm.nb(single_quest_count ~ Device + offset(log(word_count)), data=corpus)
summary(single_questmdlnb)

# Check that indeed over-dispersed:
odTest(single_questmdlnb)
single_questest <- cbind(Estimate = coef(single_questmdlnb), confint(single_questmdlnb))
exp(single_questest)





context_cue <- corpus %>%
  dplyr::select(c(Device, Period, Comma))

c <- context_cue %>% 
  group_by(Device) %>% 
  summarise(across(where(is.numeric), sum))

longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")

# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 1000 / 10897628  , 
                                   longer_c$shortening_frequency * 1000 / 3531794)
longer_c %>% 
  dplyr::select(Device, shortening, freq_normalised) %>%
  pivot_wider(names_from=Device, values_from=freq_normalised)


ggplot(longer_c, aes(y=freq_normalised, x=shortening, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


context_cue <- corpus %>%
  dplyr::select(c(Device, single_excl_count, single_quest_count, excl_count, quest_count))


c <- context_cue %>% 
  group_by(Device) %>% 
  summarise(across(where(is.numeric), sum))

longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")

# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 1000 / 10897628  , 
                                   longer_c$shortening_frequency * 1000 / 3531794)
longer_c %>% 
  dplyr::select(Device, shortening, freq_normalised) %>%
  pivot_wider(names_from=Device, values_from=freq_normalised)


longer_c$shortening <- factor(longer_c$shortening, levels = c("single_excl_count", "single_quest_count", "excl_count", "quest_count"))

ggplot(longer_c, aes(y=freq_normalised, x=shortening, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  scale_x_discrete(labels=c("excl_count"="<!!+>", "quest_count"="<??+>",
                            "single_excl_count"="<!>", "single_quest_count"="<?>"))+
  labs(x="",
       y = "Frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

