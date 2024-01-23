
# 5. QUESTION

corpus$question_token <- str_extract_all(corpus$content_clean, "\\?")
corpus$question_count <- str_count(corpus$content_clean, "\\?")


question_corp <- corpus %>% subset(question_count > 0)
question_corp <- question_corp %>%
  group_by(Device) %>%
  summarise(question_freq = sum(question_count))
question_corp$freq_normalised <- ifelse(question_corp$Device == "Phone", question_corp$question_freq * 1000000 / 9025218, 
                                        question_corp$question_freq * 1000000 / 2906358)  
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
corpus$quest_count <- str_count(corpus$content_clean, "(?<!\\?)\\?")


quest_corp <- corpus %>% subset(quest_count > 0)
quest_corp <- quest_corp %>%
  group_by(Device) %>%
  summarise(quest_freq = sum(quest_count))
quest_corp$freq_normalised <- ifelse(quest_corp$Device == "Phone", quest_corp$quest_freq * 1000000 / 9025218, 
                                     quest_corp$quest_freq * 1000000 / 2906358)  
quest_corp

# Statistics attempt 2:

questmdlnb <- glm.nb(quest_count ~ Device + offset(log(word_count)), data=corpus)
summary(questmdlnb)

# Check that indeed over-dispersed:
odTest(questmdlnb)
questest <- cbind(Estimate = coef(questmdlnb), confint(questmdlnb))
exp(questest)






# GROUPS CONTINUED: WHAT IS AVERAGE OF GROUP?

corpus$quest_ellipsis_token <- str_extract_all(corpus$content_clean, "(\\?\\?)\\?*")
corpus$quest_ellipsis_count <- str_count(corpus$content_clean, "(\\?\\?)\\?*")



quest_ellipsis_corp <- corpus %>% subset(quest_ellipsis_count > 0)
quest_ellipsis_corp <- quest_ellipsis_corp %>%
  group_by(Device) %>%
  summarise(quest_ellipsis_freq = sum(quest_ellipsis_count))
quest_ellipsis_corp$freq_normalised <- ifelse(quest_ellipsis_corp$Device == "Phone", quest_ellipsis_corp$quest_ellipsis_freq * 1000000 / 9025218, 
                                             quest_ellipsis_corp$quest_ellipsis_freq * 1000000 / 2906358)  
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
single_quest_corp$freq_normalised <- ifelse(single_quest_corp$Device == "Phone", single_quest_corp$single_quest_freq * 1000000 / 9025218, 
                                           single_quest_corp$single_quest_freq * 1000000 / 2906358)  
single_quest_corp

# Statistics attempt 2:

single_questmdlnb <- glm.nb(single_quest_count ~ Device + offset(log(word_count)), data=corpus)
summary(single_questmdlnb)

# Check that indeed over-dispersed:
odTest(single_questmdlnb)
single_questest <- cbind(Estimate = coef(single_questmdlnb), confint(single_questmdlnb))
exp(single_questest)

