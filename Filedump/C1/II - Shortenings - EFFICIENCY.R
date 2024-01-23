# II - Shortenings ATTEMPT 2.

library(tidyverse)
library(tidytext)
library(stringi)
library(ggplot2)
library(brms)
library(effects)
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/LTC")
corpus <- read_csv2("twitter_big_corpus_clean.csv")

# 1. IMO

imo <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, imo)) %>% 
  subset(imo > 0) %>% 
  uncount(imo)
imo$shortening <- "short"
inmyopinion <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, inmyopinion)) %>% 
  subset(inmyopinion > 0) %>% 
  uncount(inmyopinion)
inmyopinion$shortening <- "full"
imo <- rbind(imo, inmyopinion)
imo$shortening <- as.factor(imo$shortening)

imo %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)

imomdl <- glm(shortening ~ Device, data = imo, family = "binomial")

summary(imomdl)
exp(cbind(OR = coef(imomdl), confint(imomdl)))

# 2. btw

btw <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, btw)) %>% 
  subset(btw > 0) %>% 
  uncount(btw)
btw$shortening <- "short"
bytheway <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, bytheway)) %>% 
  subset(bytheway > 0) %>% 
  uncount(bytheway)
bytheway$shortening <- "full"
btw <- rbind(btw, bytheway)
btw$shortening <- as.factor(btw$shortening)

btw %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)

btwmdl <- glm(shortening ~ Device, data = btw, family = "binomial")

summary(btwmdl)
summary_btw
exp(cbind(OR = coef(btwmdl), confint(btwmdl)))


# 3. TBH

tbh <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, tbh)) %>% 
  subset(tbh > 0) %>% 
  uncount(tbh)
tbh$shortening <- "short"
tobehonest <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, tobehonest)) %>% 
  subset(tobehonest > 0) %>% 
  uncount(tobehonest)
tobehonest$shortening <- "full"
tbh <- rbind(tbh, tobehonest)
tbh$shortening <- as.factor(tbh$shortening)

summary_tbh <- tbh %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)

tbhmdl <- glm(shortening ~ Device, data = tbh, family = "binomial")

summary(tbhmdl)
summary_tbh
exp(cbind(OR = coef(tbhmdl), confint(tbhmdl)))

# 4. TBF

tbf <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, tbf)) %>% 
  
  subset(tbf> 0) %>% 
  uncount( tbf)
tbf$shortening <- "short"
tobefair <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, tobefair)) %>% 
  
  subset(tobefair> 0) %>% 
  uncount( tobefair)
tobefair$shortening <- "full"
tbf <- rbind(tbf, tobefair)
tbf$shortening <- as.factor(tbf$shortening)

summary_tbf <- tbf %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_tbf
summary_tbf %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

tbfmdl <- glm(shortening ~ Device, data = tbf, family = "binomial")

summary_tbf
exp(cbind(OR = coef(tbfmdl), confint(tbfmdl)))
summary(tbfmdl)

effect("Device", tbfmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# RN

rn <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, rn)) %>% 
  
  subset(rn> 0) %>% 
  uncount( rn)
rn$shortening <- "short"
rightnow <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, rightnow)) %>% 
  
  subset(rightnow> 0) %>% 
  uncount( rightnow)
rightnow$shortening <- "full"
rn <- rbind(rn, rightnow)
rn$shortening <- as.factor(rn$shortening)

summary_rn <- rn %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_rn
summary_rn %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

rnmdl <- glm(shortening ~ Device, data = rn, family = "binomial")

summary_rn
exp(cbind(OR = coef(rnmdl), confint(rnmdl)))
summary(rnmdl)

effect("Device", rnmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")


# DW

dw <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, dw)) %>% 
  
  subset(dw> 0) %>% 
  uncount( dw)
dw$shortening <- "short"
dontworry <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, dontworry)) %>% 
  
  subset(dontworry> 0) %>% 
  uncount( dontworry)
dontworry$shortening <- "full"
dw <- rbind(dw, dontworry)
dw$shortening <- as.factor(dw$shortening)

summary_dw <- dw %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_dw
summary_dw %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

dwmdl <- glm(shortening ~ Device, data = dw, family = "binomial")

summary_dw
exp(cbind(OR = coef(dwmdl), confint(dwmdl)))
summary(dwmdl)

effect("Device", dwmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")


# OTOH

otoh <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, otoh)) %>% 
  
  subset(otoh> 0) %>% 
  uncount( otoh)
otoh$shortening <- "short"
ontheotherhand <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, ontheotherhand)) %>% 
  
  subset(ontheotherhand> 0) %>% 
  uncount( ontheotherhand)
ontheotherhand$shortening <- "full"
otoh <- rbind(otoh, ontheotherhand)
otoh$shortening <- as.factor(otoh$shortening)

summary_otoh <- otoh %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_otoh
summary_otoh %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

otohmdl <- glm(shortening ~ Device, data = otoh, family = "binomial")

summary_otoh
exp(cbind(OR = coef(otohmdl), confint(otohmdl)))
summary(otohmdl)

effect("Device", otohmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# WTF

wtf <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, wtf)) %>% 
  
  subset(wtf> 0) %>% 
  uncount( wtf)
wtf$shortening <- "short"
whatthefuck <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, whatthefuck)) %>% 
  
  subset(whatthefuck> 0) %>% 
  uncount( whatthefuck)
whatthefuck$shortening <- "full"
wtf <- rbind(wtf, whatthefuck)
wtf$shortening <- as.factor(wtf$shortening)

summary_wtf <- wtf %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_wtf
summary_wtf %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

wtfmdl <- glm(shortening ~ Device, data = wtf, family = "binomial")

summary_wtf
exp(cbind(OR = coef(wtfmdl), confint(wtfmdl)))
summary(wtfmdl)

effect("Device", wtfmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# omg

omg <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, omg)) %>% 
  
  subset(omg> 0) %>% 
  uncount( omg)
omg$shortening <- "short"
ohmygod <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, ohmygod)) %>% 
  
  subset(ohmygod> 0) %>% 
  uncount( ohmygod)
ohmygod$shortening <- "full"
omg <- rbind(omg, ohmygod)
omg$shortening <- as.factor(omg$shortening)

summary_omg <- omg %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_omg
summary_omg %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

omgmdl <- glm(shortening ~ Device, data = omg, family = "binomial")

summary_omg
exp(cbind(OR = coef(omgmdl), confint(wtfmdl)))
summary(omgmdl)

effect("Device", omgmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# idk

idk <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, idk)) %>% 
  
  subset(idk> 0) %>% 
  uncount( idk)
idk$shortening <- "short"
idontknow <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, idontknow)) %>% 
  
  subset(idontknow> 0) %>% 
  uncount( idontknow)
idontknow$shortening <- "full"
idk <- rbind(idk, idontknow)
idk$shortening <- as.factor(idk$shortening)

summary_idk <- idk %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_idk
summary_idk %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

idkmdl <- glm(shortening ~ Device, data = idk, family = "binomial")

summary_idk
exp(cbind(OR = coef(idkmdl), confint(idkmdl)))
summary(idkmdl)

effect("Device", idkmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# IKR

ikr <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, ikr)) %>% 
  
  subset(ikr> 0) %>% 
  uncount( ikr)
ikr$shortening <- "short"
iknowright <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, iknowright)) %>% 
  
  subset(iknowright> 0) %>% 
  uncount( iknowright)
iknowright$shortening <- "full"
ikr <- rbind(ikr, iknowright)
ikr$shortening <- as.factor(ikr$shortening)

summary_ikr <- ikr %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_ikr
summary_ikr %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

ikrmdl <- glm(shortening ~ Device, data = ikr, family = "binomial")

summary_ikr
exp(cbind(OR = coef(ikrmdl), confint(ikrmdl)))
summary(ikrmdl)

effect("Device", ikrmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# ESP

esp <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, esp)) %>% 
  
  subset(esp> 0) %>% 
  uncount( esp)
esp$shortening <- "short"
especially <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, especially)) %>% 
  
  subset(especially> 0) %>% 
  uncount( especially)
especially$shortening <- "full"
esp <- rbind(esp, especially)
esp$shortening <- as.factor(esp$shortening)

summary_esp <- esp %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_esp
summary_esp %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

espmdl <- glm(shortening ~ Device, data = esp, family = "binomial")

summary_esp
exp(cbind(OR = coef(espmdl), confint(espmdl)))
summary(espmdl)

effect("Device", espmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# DEF

def <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, def)) %>% 
  
  subset(def> 0) %>% 
  uncount( def)
def$shortening <- "short"
definitely <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, definitely)) %>% 
  
  subset(definitely> 0) %>% 
  uncount( definitely)
definitely$shortening <- "full"
def <- rbind(def, definitely)
def$shortening <- as.factor(def$shortening)

summary_def <- def %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_def
summary_def %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

defmdl <- glm(shortening ~ Device, data = def, family = "binomial")

summary_def
exp(cbind(OR = coef(defmdl), confint(defmdl)))
summary(defmdl)

effect("Device", defmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# PPL

ppl <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, ppl)) %>% 
  
  subset(ppl> 0) %>% 
  uncount( ppl)
ppl$shortening <- "short"
people <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, people)) %>% 
  
  subset(people> 0) %>% 
  uncount( people)
people$shortening <- "full"
ppl <- rbind(ppl, people)
ppl$shortening <- as.factor(ppl$shortening)

summary_ppl <- ppl %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_ppl
summary_ppl %>%
  group_by(Device) %>%
  summarise(mean_shortened = mean(percent)) %>%
  ggplot(aes(x = Device,
             y = mean_shortened)) +
  geom_col() +
  geom_label(aes(label = round(mean_shortened, 
                               digits = 2)))

pplmdl <- glm(shortening ~ Device, data = ppl, family = "binomial")

summary_ppl
exp(cbind(OR = coef(pplmdl), confint(pplmdl)))
summary(pplmdl)

effect("Device", pplmdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# STFU

stfu <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, stfu)) %>% 
  
  subset(stfu> 0) %>% 
  uncount( stfu)
stfu$shortening <- "short"
shutthefuckup <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, shutthefuckup)) %>% 
  
  subset(shutthefuckup> 0) %>% 
  uncount( shutthefuckup)
shutthefuckup$shortening <- "full"
stfu <- rbind(stfu, shutthefuckup)
stfu$shortening <- as.factor(stfu$shortening)

summary_stfu <- stfu %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_stfu

stfumdl <- glm(shortening ~ Device, data = stfu, family = "binomial")

summary_stfu
exp(cbind(OR = coef(stfumdl), confint(stfumdl)))
summary(stfumdl)

effect("Device", stfumdl) %>%
  data.frame() %>%
  ggplot(aes(x = reorder(Device, fit),
             y = fit)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = .2) +
  geom_label(aes(label = round(fit, digits = 2)))  +
  labs(x = "effect of device")

# af

af <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, af)) %>% 
  subset(af> 0) %>% 
  uncount( af)
af$shortening <- "short"
asfuck <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, asfuck)) %>% 
  subset(asfuck> 0) %>% 
  uncount( asfuck)
asfuck$shortening <- "full"
af <- rbind(af, asfuck)
af$shortening <- as.factor(af$shortening)

summary_af <- af %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_af

afmdl <- glm(shortening ~ Device, data = af, family = "binomial")

exp(cbind(OR = coef(afmdl), confint(afmdl)))
summary(afmdl)

# ffs

ffs <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, ffs)) %>% 
  subset(ffs> 0) %>% 
  uncount( ffs)
ffs$shortening <- "short"
forfuckssake <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, forfuckssake)) %>% 
  subset(forfuckssake> 0) %>% 
  uncount( forfuckssake)
forfuckssake$shortening <- "full"
ffs <- rbind(ffs, forfuckssake)
ffs$shortening <- as.factor(ffs$shortening)

summary_ffs <- ffs %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_ffs

ffsmdl <- glm(shortening ~ Device, data = ffs, family = "binomial")

exp(cbind(OR = coef(ffsmdl), confint(ffsmdl)))
summary(ffsmdl)

# ily

ily <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, ily)) %>% 
  subset(ily> 0) %>% 
  uncount( ily)
ily$shortening <- "short"
iloveyou <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, iloveyou)) %>% 
  subset(iloveyou> 0) %>% 
  uncount( iloveyou)
iloveyou$shortening <- "full"
ily <- rbind(ily, iloveyou)
ily$shortening <- as.factor(ily$shortening)

summary_ily <- ily %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_ily

ilymdl <- glm(shortening ~ Device, data = ily, family = "binomial")

exp(cbind(OR = coef(ilymdl), confint(ilymdl)))
summary(ilymdl)

# GTFO

gtfo <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, gtfo)) %>% 
  subset(gtfo> 0) %>% 
  uncount( gtfo)
gtfo$shortening <- "short"
getthefuckout <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, getthefuckout)) %>% 
  subset(getthefuckout> 0) %>% 
  uncount( getthefuckout)
getthefuckout$shortening <- "full"
gtfo <- rbind(gtfo, getthefuckout)
gtfo$shortening <- as.factor(gtfo$shortening)

summary_gtfo <- gtfo %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_gtfo

gtfomdl <- glm(shortening ~ Device, data = gtfo, family = "binomial")

exp(cbind(OR = coef(gtfomdl), confint(gtfomdl)))
summary(gtfomdl)

# ur

ur <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, ur)) %>% 
  subset(ur> 0) %>% 
  uncount( ur)
ur$shortening <- "short"
your <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, your)) %>% 
  subset(your> 0) %>% 
  uncount( your)
your$shortening <- "full"
ur <- rbind(ur, your)
ur$shortening <- as.factor(ur$shortening)

summary_ur <- ur %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_ur

urmdl <- glm(shortening ~ Device, data = ur, family = "binomial")

exp(cbind(OR = coef(urmdl), confint(urmdl)))
summary(urmdl)

# u

u <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, u)) %>% 
  subset(u> 0) %>% 
  uncount( u)
u$shortening <- "short"
you <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, you)) %>% 
  subset(you> 0) %>% 
  uncount( you)
you$shortening <- "full"
u <- rbind(u, you)
u$shortening <- as.factor(u$shortening)

summary_u <- u %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_u

umdl <- glm(shortening ~ Device, data = u, family = "binomial")

exp(cbind(OR = coef(umdl), confint(umdl)))
summary(umdl)


#sry
sry <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, sry)) %>% 
  subset(sry> 0) %>% 
  uncount( sry)
sry$shortening <- "short"
sorry <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, sorry)) %>% 
  subset(sorry> 0) %>% 
  uncount( sorry)
sorry$shortening <- "full"
sry <- rbind(sry, sorry)
sry$shortening <- as.factor(sry$shortening)

summary_sry <- sry %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_sry

srymdl <- glm(shortening ~ Device, data = sry, family = "binomial")

exp(cbind(OR = coef(srymdl), confint(srymdl)))
summary(srymdl)


# pls

pls <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, pls)) %>% 
  subset(pls> 0) %>% 
  uncount( pls)
pls$shortening <- "short"
please <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, please)) %>% 
  subset(please> 0) %>% 
  uncount( please)
please$shortening <- "full"
pls <- rbind(pls, please)
pls$shortening <- as.factor(pls$shortening)

summary_pls <- pls %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_pls

plsmdl <- glm(shortening ~ Device, data = pls, family = "binomial")

exp(cbind(OR = coef(plsmdl), confint(plsmdl)))
summary(plsmdl)


# ofc

ofc <- corpus %>% 
  dplyr::select(c(Author, Device, content_clean, ofc)) %>% 
  subset(ofc> 0) %>% 
  uncount( ofc)
ofc$shortening <- "short"
ofcourse <- corpus %>%
  dplyr::select(c(Author, Device, content_clean, ofcourse)) %>% 
  subset(ofcourse> 0) %>% 
  uncount( ofcourse)
ofcourse$shortening <- "full"
ofc <- rbind(ofc, ofcourse)
ofc$shortening <- as.factor(ofc$shortening)

summary_ofc <- ofc %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_ofc

ofcmdl <- glm(shortening ~ Device, data = ofc, family = "binomial")

exp(cbind(OR = coef(ofcmdl), confint(ofcmdl)))
summary(ofcmdl)