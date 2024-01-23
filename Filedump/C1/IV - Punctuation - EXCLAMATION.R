# EXCLAMATION MARKS

corpus$exclamation_token <- str_extract_all(corpus$content_clean, "\\!")
corpus$exclamation_count <- str_count(corpus$content_clean, "\\!")


exclamation_corp <- corpus %>% subset(exclamation_count > 0)
exclamation_corp <- exclamation_corp %>%
  group_by(Device) %>%
  summarise(exclamation_freq = sum(exclamation_count))
exclamation_corp$freq_normalised <- ifelse(exclamation_corp$Device == "Phone", exclamation_corp$exclamation_freq * 1000000 / 9025218, 
                                           exclamation_corp$exclamation_freq * 1000000 / 2906358)  
exclamation_corp


# Statistics attempt:

corpus <- corpus %>% subset(word_count> 0)

exclamationmdlnb <- glm.nb(exclamation_count ~ Device + offset(log(word_count)), data=corpus)
exclamationmdlnb2 <- glm.nb(exclamation_count ~ Device, data=corpus)
summary(exclamationmdlnb)
summary(exclamationmdlnb2)

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


corpus$excl_token <- str_extract_all(corpus$content_clean, "(?<!\\!)\\!")
corpus$excl_count <- str_count(corpus$content_clean, "(?<!\\!)\\!")


excl_corp <- corpus %>% subset(excl_count > 0)
excl_corp <- excl_corp %>%
  group_by(Device) %>%
  summarise(excl_freq = sum(excl_count))
excl_corp$freq_normalised <- ifelse(excl_corp$Device == "Phone", excl_corp$excl_freq * 1000000 / 9025218, 
                                    excl_corp$excl_freq * 1000000 / 2906358)  
excl_corp

# Statistics attempt:

exclmdlnb <- glm.nb(excl_count ~ Device + offset(log(word_count)), data=corpus)
summary(exclmdlnb)

# Check that indeed over-dispersed:
odTest(exclmdlnb)
exclest <- cbind(Estimate = coef(exclmdlnb), confint(exclmdlnb))
exp(exclest)



# GROUPS CONTINUED: WHAT IS AVERAGE OF GROUP?

corpus$excl_ellipsis_token <- str_extract_all(corpus$content_clean, "(\\!\\!)\\!*")
corpus$excl_ellipsis_count <- str_count(corpus$content_clean, "(\\!\\!)\\!*")



excl_ellipsis_corp <- corpus %>% subset(excl_ellipsis_count > 0)
excl_ellipsis_corp <- excl_ellipsis_corp %>%
  group_by(Device) %>%
  summarise(excl_ellipsis_freq = sum(excl_ellipsis_count))
excl_ellipsis_corp$freq_normalised <- ifelse(excl_ellipsis_corp$Device == "Phone", excl_ellipsis_corp$excl_ellipsis_freq * 1000000 / 9025218, 
                                        excl_ellipsis_corp$excl_ellipsis_freq * 1000000 / 2906358)  
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
single_excl_corp$freq_normalised <- ifelse(single_excl_corp$Device == "Phone", single_excl_corp$single_excl_freq * 1000000 / 9025218, 
                                      single_excl_corp$single_excl_freq * 1000000 / 2906358)  
single_excl_corp

# Statistics attempt 2:

single_exclmdlnb <- glm.nb(single_excl_count ~ Device + offset(log(word_count)), data=corpus)
summary(single_exclmdlnb)

# Check that indeed over-dispersed:
odTest(single_exclmdlnb)
single_exclest <- cbind(Estimate = coef(single_exclmdlnb), confint(single_exclmdlnb))
exp(single_exclest)

