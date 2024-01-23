#  Shortenings - Context cues

library(tidyverse)
library(MASS)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(pscl)

# Do Shortenings.R first
corpus <- corpus %>%
  dplyr::select(c(ID, Author, Device, Content, content_clean, word_count, idk, lol, lmao, omg, smh, wtf))


# PREP DATA:
# Delete all comments with 0 word count - otherwise offset doesn't work!
corpus <- corpus %>% subset(corpus$word_count > 0)

# Frequency across device, per words
c <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(wtf,idk,omg,smh,lol,lmao), sum))
longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")
# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 100000 / 10897628  , 
                                   longer_c$shortening_frequency * 100000 / 3531794)


# Frequency across device, per words
c <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(af, btw, dw, ffs, gtfo, idk, ikr, ily, imo, omg, otoh, rn, stfu, tbf, tbh, wtf), sum))
longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")
# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 100000 / 10897628  , 
                                   longer_c$shortening_frequency * 100000 / 3531794)

longer_c

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Graph - Chap 5 - Microlinguistic features v1")
write_csv2(longer_c, "normalised_acronyms_phrase_frequencies.csv")


c <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(def, esp, ofc, pls, ppl, sry, u, ur), sum))
longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")
# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 100000 / 10897628  , 
                                   longer_c$shortening_frequency * 100000 / 3531794)

longer_c

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/Graphs/Graph - Chap 5 - Microlinguistic features v1")
write_csv2(longer_c, "normalised_acronyms_phrase_frequencies_2.csv")


# IDK

idkmdlnb <- glm.nb(idk ~ Device + offset(log(word_count)), data=corpus)
summary(idkmdlnb)

# Check that indeed over-dispersed:
odTest(idkmdlnb)
estidk <- cbind(Estimate = coef(idkmdlnb), confint(idkmdlnb))
exp(estidk)


# LOL

lolmdlnb <- glm.nb(lol ~ Device + offset(log(word_count)), data=corpus)
summary(lolmdlnb)

# Check that indeed over-dispersed:
odTest(lolmdlnb)
estlol <- cbind(Estimate = coef(lolmdlnb), confint(lolmdlnb))
exp(estlol)


# LMAO

lmaomdlnb <- glm.nb(lmao ~ Device + offset(log(word_count)), data=corpus)
summary(lmaomdlnb)

# Check that indeed over-dispersed:
odTest(lmaomdlnb)
lmaoest <- cbind(Estimate = coef(lmaomdlnb), confint(lmaomdlnb))
exp(lmaoest)


# OMG

omgmdlnb <- glm.nb(omg ~ Device + offset(log(word_count)), data=corpus)
summary(omgmdlnb)

# Check that indeed over-dispersed:
odTest(omgmdlnb)
omgmdlnb <- cbind(Estimate = coef(omgmdlnb), confint(omgmdlnb))
exp(omgmdlnb)


# SMH

smhmdlnb <- glm.nb(smh ~ Device + offset(log(word_count)), data=corpus)
summary(smhmdlnb)

# Check that indeed over-dispersed:
odTest(smhmdlnb)
smhmdlnb <- cbind(Estimate = coef(smhmdlnb), confint(smhmdlnb))
exp(smhmdlnb)


# WTF

wtfmdlnb <- glm.nb(wtf ~ Device + offset(log(word_count)), data=corpus)
summary(wtfmdlnb)

# Check that indeed over-dispersed:
odTest(wtfmdlnb)
wtfmdlnb <- cbind(Estimate = coef(wtfmdlnb), confint(wtfmdlnb))
exp(wtfmdlnb)


# AF

afmdlnb <- glm(af ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(afmdlnb)

# Check that indeed over-dispersed:
odTest(afmdlnb)
afmdlnb <- cbind(Estimate = coef(afmdlnb), confint(afmdlnb))
exp(afmdlnb)


# BTW

btwmdlnb <- glm.nb(btw ~ Device + offset(log(word_count)), data=corpus)
summary(btwmdlnb)

# Check that indeed over-dispersed:
odTest(btwmdlnb)
btwmdlnb <- cbind(Estimate = coef(btwmdlnb), confint(btwmdlnb))
exp(btwmdlnb)


# dw

dwmdlnb <- glm.nb(dw ~ Device + offset(log(word_count)), data=corpus)
summary(dwmdlnb)

# Check that indeed over-dispersed:
odTest(dwmdlnb)
dwmdlnb <- glm(dw ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
dwmdlnb <- cbind(Estimate = coef(dwmdlnb), confint(dwmdlnb))
exp(dwmdlnb)


# ffs

ffsmdlnb <- glm.nb(ffs ~ Device + offset(log(word_count)), data=corpus)
odTest(ffsmdlnb)
ffsmdlnb <- glm(ffs ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(ffsmdlnb)

# Check that indeed over-dispersed:
ffsmdlnb <- cbind(Estimate = coef(ffsmdlnb), confint(ffsmdlnb))
exp(ffsmdlnb)


# gtfo

gtfomdlnb <- glm.nb(gtfo ~ Device + offset(log(word_count)), data=corpus)
odTest(gtfomdlnb)
gtfomdlnb <- glm(gtfo ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(gtfomdlnb)

# Check that indeed over-dispersed:
gtfomdlnb <- cbind(Estimate = coef(gtfomdlnb), confint(gtfomdlnb))
exp(gtfomdlnb)


# ikr

ikrmdlnb <- glm.nb(ikr ~ Device + offset(log(word_count)), data=corpus)
odTest(ikrmdlnb)
ikrmdlnb <- glm(ikr ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(ikrmdlnb)

# Check that indeed over-dispersed:
ikrmdlnb <- cbind(Estimate = coef(ikrmdlnb), confint(ikrmdlnb))
exp(ikrmdlnb)


# ily

ilymdlnb <- glm.nb(ily ~ Device + offset(log(word_count)), data=corpus)
odTest(ilymdlnb)
summary(ilymdlnb)

# Check that indeed over-dispersed:
ilymdlnb <- cbind(Estimate = coef(ilymdlnb), confint(ilymdlnb))
exp(ilymdlnb)


# imo

imomdlnb <- glm.nb(imo ~ Device + offset(log(word_count)), data=corpus)
odTest(imomdlnb)
summary(imomdlnb)

# Check that indeed over-dispersed:
imomdlnb <- cbind(Estimate = coef(imomdlnb), confint(imomdlnb))
exp(imomdlnb)


# otoh

otohmdlnb <- glm.nb(otoh ~ Device + offset(log(word_count)), data=corpus)
odTest(otohmdlnb)
otohmdlnb <- glm(otoh ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(otohmdlnb)

# Check that indeed over-dispersed:
otohmdlnb <- cbind(Estimate = coef(otohmdlnb), confint(otohmdlnb))
exp(otohmdlnb)


# rn

rnmdlnb <- glm.nb(rn ~ Device + offset(log(word_count)), data=corpus)
odTest(rnmdlnb)
summary(rnmdlnb)

# Check that indeed over-dispersed:
rnmdlnb <- cbind(Estimate = coef(rnmdlnb), confint(rnmdlnb))
exp(rnmdlnb)


# stfu

stfumdlnb <- glm.nb(stfu ~ Device + offset(log(word_count)), data=corpus)
odTest(stfumdlnb)
stfumdlnb <- glm(stfu ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(stfumdlnb)

# Check that indeed over-dispersed:
stfumdlnb <- cbind(Estimate = coef(stfumdlnb), confint(stfumdlnb))
exp(stfumdlnb)


# tbf

tbfmdlnb <- glm.nb(tbf ~ Device + offset(log(word_count)), data=corpus)
odTest(tbfmdlnb)
tbfmdlnb <- glm(tbf ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(tbfmdlnb)

# Check that indeed over-dispersed:
tbfmdlnb <- cbind(Estimate = coef(tbfmdlnb), confint(tbfmdlnb))
exp(tbfmdlnb)


# tbh

tbhmdlnb <- glm.nb(tbh ~ Device + offset(log(word_count)), data=corpus)
odTest(tbhmdlnb)
tbhmdlnb <- glm(tbh ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(tbhmdlnb)

# Check that indeed over-dispersed:
tbhmdlnb <- cbind(Estimate = coef(tbhmdlnb), confint(tbhmdlnb))
exp(tbhmdlnb)




# one word (abbreviations)


# def

defmdlnb <- glm.nb(def ~ Device + offset(log(word_count)), data=corpus)
odTest(defmdlnb)
summary(defmdlnb)

# Check that indeed over-dispersed:
defmdlnb <- cbind(Estimate = coef(defmdlnb), confint(defmdlnb))
exp(defmdlnb)


# esp

espmdlnb <- glm.nb(esp ~ Device + offset(log(word_count)), data=corpus)
odTest(espmdlnb)
summary(espmdlnb)

# Check that indeed over-dispersed:
espmdlnb <- cbind(Estimate = coef(espmdlnb), confint(espmdlnb))
exp(espmdlnb)


# ppl

pplmdlnb <- glm.nb(ppl ~ Device + offset(log(word_count)), data=corpus)
odTest(pplmdlnb)
summary(pplmdlnb)

# Check that indeed over-dispersed:
pplmdlnb <- cbind(Estimate = coef(pplmdlnb), confint(pplmdlnb))
exp(pplmdlnb)


# sry

srymdlnb <- glm.nb(sry ~ Device + offset(log(word_count)), data=corpus)
odTest(srymdlnb)
srymdlnb <- glm(sry ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(srymdlnb)

# Check that indeed over-dispersed:
srymdlnb <- cbind(Estimate = coef(srymdlnb), confint(srymdlnb))
exp(srymdlnb)


# u

umdlnb <- glm.nb(u ~ Device + offset(log(word_count)), data=corpus)
odTest(umdlnb)
summary(umdlnb)

# Check that indeed over-dispersed:
umdlnb <- cbind(Estimate = coef(umdlnb), confint(umdlnb))
exp(umdlnb)


# ur

urmdlnb <- glm.nb(ur ~ Device + offset(log(word_count)), data=corpus)
odTest(urmdlnb)
summary(urmdlnb)

# Check that indeed over-dispersed:
urmdlnb <- cbind(Estimate = coef(urmdlnb), confint(urmdlnb))
exp(urmdlnb)


# pls

plsmdlnb <- glm.nb(pls ~ Device + offset(log(word_count)), data=corpus)
summary(plsmdlnb)

# Check that indeed over-dispersed:
odTest(plsmdlnb)
estpls <- cbind(Estimate = coef(plsmdlnb), confint(plsmdlnb))
exp(estpls)

# ofc
#poisson!
ofcmdlnb <- glm(ofc ~ Device + offset(log(word_count)), family = "poisson", data=corpus)
summary(ofcmdlnb)

# Check that indeed over-dispersed:
odTest(ofcmdlnb)
estofc <- cbind(Estimate = coef(ofcmdlnb), confint(ofcmdlnb))
exp(estofc)