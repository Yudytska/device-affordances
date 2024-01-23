# Add inferential stats
library(lme4)


corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast", "general", "the-tome-zone"))
corpus$Participant <- as.factor(corpus$Participant)
wcmdlnb <- glm.nb(word_count ~ Device * Channel , data=corpus)
summary(wcmdlnb)
est <- cbind(Estimate = coef(wcmdlnb), confint(wcmdlnb))
exp(est)

p_mfit<-glmer(word_count ~ Device + Platform + (1|Participant), family="poisson", data=corpus)

twitter_corpus <- corpus %>%
  filter(Channel %in% c("broadcast", "narrowcast"))
discord_corpus <- corpus %>%
  filter(Channel %in% c("general", "the-tome-zone"))


disc_wcmdlnb <- glm.nb(word_count ~ Device * Channel, data=discord_corpus)
summary(disc_wcmdlnb)
est <- cbind(Estimate = coef(disc_wcmdlnb), confint(disc_wcmdlnb))
exp(est)


twit_wcmdlnb <- glm.nb(word_count ~ Device * Channel, data=twitter_corpus)
summary(twit_wcmdlnb)
est <- cbind(Estimate = coef(twit_wcmdlnb), confint(twit_wcmdlnb))
exp(est)

lineplot.CI( x.factor = twitter_corpus$Channel, 
             response = twitter_corpus$word_count,
             group = twitter_corpus$Device,
             ci.fun = ciMean,
             xlab = "Device",
             ylab = "TUs with emojis (%)",
             col=c("black","red"),  ### Colors for levels of trace var.
             #pch=c(19, 17, 15),             ### Symbols for levels of trace var.
             fixed=TRUE,                    ### Order by factor order in data
             leg.bty = "o")




p_mfit<-glmer.nb(word_count ~ Device * Platform + (1|Participant),  data=corpus)
summary(p_mfit)
est <- cbind(Estimate = coef(p_mfit), confint(p_mfit))
exp(est)

p_mfit<-glmer.nb(word_count ~ Device * Channel + (1|Participant),  data=discord_corpus)
summary(p_mfit)

p_mfit<-glmer.nb(word_count ~ Device * Channel + (1|Participant),  data=twitter_corpus)
summary(p_mfit)

