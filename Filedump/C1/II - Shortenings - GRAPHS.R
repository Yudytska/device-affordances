# II. Shortenings - Part 2, graphing
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# Do Shortenings.R first.

# EFFICIENCY-BASED: Look as comparison between acronym and full form
corpus <- corpus %>% subset(word_count > 0)

a <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(af, btw, ffs, idk, ily, imo, omg, rn,  tbh, wtf), sum))

longer_a <- a %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")


# Frequency as % of (acronym + full form)

b <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(asfuck, bytheway,forfuckssake, idontknow, 
                     iloveyou, inmyopinion, ohmygod, rightnow, 
                     tobehonest, whatthefuck), sum))

longer_b <- b %>%
  pivot_longer(!Device, names_to = "full_form", values_to = "full_form_frequency")

combo_frequency <- longer_a
combo_frequency$full_form <- longer_b$full_form
combo_frequency$full_form_frequency <- longer_b$full_form_frequency

combo_frequency2 <- combo_frequency
combo_frequency2 <- dplyr::select(combo_frequency2, -full_form)

combo_frequency2

combo_frequency3 <- combo_frequency2 %>%
  gather(shortened, frequency, shortening_frequency:full_form_frequency)
combo_frequency3$shortened <- ifelse(combo_frequency3$shortened == "shortening_frequency",
                                     "shortening", "full phrase")
combo_frequency3$shortened <- as.factor(combo_frequency3$shortened)
combo_frequency3$shortening <- as.factor(combo_frequency3$shortening)
combo_frequency3$Device <- as.factor(combo_frequency3$Device)

combo_frequency3$shortened <- ifelse(combo_frequency3$shortened == "shortening",
                                     "Acronym", "Full phrase")
combo_frequency3$shortened <- factor(combo_frequency3$shortened,
                                     levels = c("Full phrase", "Acronym"))


combo_frequency3$frequency <- as.numeric(combo_frequency3$frequency)

ggplot(combo_frequency3, aes(fill=shortened, y=frequency, x=Device)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~shortening) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="",
       y = "", fill = "Potential acronym")


a <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(ofc, pls, ppl, u,ur), sum))

longer_a <- a %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")


# Frequency as % of (acronym + full form)

b <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(ofcourse, please, people, you, your), sum))

longer_b <- b %>%
  pivot_longer(!Device, names_to = "full_form", values_to = "full_form_frequency")

combo_frequency <- longer_a
combo_frequency$full_form <- longer_b$full_form
combo_frequency$full_form_frequency <- longer_b$full_form_frequency

combo_frequency2 <- combo_frequency
combo_frequency2 <- dplyr::select(combo_frequency2, -full_form)

combo_frequency2

combo_frequency3 <- combo_frequency2 %>%
  gather(shortened, frequency, shortening_frequency:full_form_frequency)
combo_frequency3$shortened <- ifelse(combo_frequency3$shortened == "shortening_frequency",
                                     "shortened", "full phrase")
combo_frequency3$shortened <- as.factor(combo_frequency3$shortened)
combo_frequency3$shortening <- as.factor(combo_frequency3$shortening)
combo_frequency3$Device <- as.factor(combo_frequency3$Device)

combo_frequency3$frequency <- as.numeric(combo_frequency3$frequency)

combo_frequency3$shortened <- ifelse(combo_frequency3$shortened == "shortened",
                                     "Abbreviation", "Full phrase")
combo_frequency3$shortened <- factor(combo_frequency3$shortened,
                                     levels = c("Full phrase", "Abbreviation"))


ggplot(combo_frequency3, aes(fill=shortened, y=frequency, x=Device)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~shortening) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="",
       y = "", fill = "Potential abbreviation")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Contextualisation cue

# WTF, IDK, OMG, SMH, LOL, LMAO

context_cue <- corpus %>%
  dplyr::select(c(Device,ofc, pls, ppl, u,ur))

c <- context_cue %>% 
  group_by(Device) %>% 
  summarise(across(where(is.numeric), sum))

longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")

# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 100000 / 10897628  , 
                                   longer_c$shortening_frequency * 100000 / 3531794)
longer_c %>% 
  dplyr::select(Device, shortening, freq_normalised) %>%
  pivot_wider(names_from=Device, values_from=freq_normalised)


ggplot(longer_c, aes(y=freq_normalised, x=shortening, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="",
       y = "Frequency (per 100,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))




context_cue <- corpus %>%
  dplyr::select(c(Device, smh, lol, lmao))

c <- context_cue %>% 
  group_by(Device) %>% 
  summarise(across(where(is.numeric), sum))

longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")

# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 100000 / 10897628  , 
                                   longer_c$shortening_frequency * 100000 / 3531794)
longer_c %>% 
  dplyr::select(Device, shortening, freq_normalised) %>%
  pivot_wider(names_from=Device, values_from=freq_normalised)


ggplot(longer_c, aes(y=freq_normalised, x=shortening, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="",
       y = "Frequency (per 100,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))




context_cue <- corpus %>%
  dplyr::select(c(Device, af, btw, ffs, idk, ily, imo, omg, rn, tbh, wtf))

c <- context_cue %>% 
  group_by(Device) %>% 
  summarise(across(where(is.numeric), sum))

longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")

# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 100000 / 10897628  , 
                                   longer_c$shortening_frequency * 100000 / 3531794)
longer_c %>% 
  dplyr::select(Device, shortening, freq_normalised) %>%
  pivot_wider(names_from=Device, values_from=freq_normalised)


ggplot(longer_c, aes(y=freq_normalised, x=shortening, fill = Device)) + 
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_brewer(palette = "Paired")+
  labs(x="",
       y = "Frequency (per 100,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

