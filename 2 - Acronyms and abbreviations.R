# 5.3.

# Code for acronyms + abbreviations + full phrases
# Note: coded for more than actually used, as some had too few tokens.


corpus$imo <- str_count(corpus$content_clean_punctuation, "\\bi+m+h*o+\\b")
corpus$btw <- str_count(corpus$content_clean_punctuation, "\\bb+t+w+\\b")
corpus$tbh <- str_count(corpus$content_clean_punctuation, "\\bt+b+h+\\b")
corpus$tbf <- str_count(corpus$content_clean_punctuation, "\\bt+b+f+\\b")
corpus$def <- str_count(corpus$content_clean_punctuation, "\\bd+e+f+\\b")
corpus$ppl <- str_count(corpus$content_clean_punctuation, "\\bp+pl+\\b")
corpus$esp <- str_count(corpus$content_clean_punctuation, "\\be+s+p+\\b")
corpus$rn <- str_count(corpus$content_clean_punctuation, "\\br+n+\\b")
corpus$dw <- str_count(corpus$content_clean_punctuation, "\\bd+w+\\b")
corpus$otoh <- str_count(corpus$content_clean_punctuation, "\\bo+t+o+h+\\b")
corpus$ikr <- str_count(corpus$content_clean_punctuation, "\\bi+k+r+\\b")
corpus$stfu <- str_count(corpus$content_clean_punctuation, "\\bs+t+f+u+\\b")
corpus$af <- str_count(corpus$content_clean_punctuation, "\\ba+f+\\b")
corpus$ffs <- str_count(corpus$content_clean_punctuation, "\\bf+f+s+\\b")
corpus$ur <- str_count(corpus$content_clean_punctuation, "\\bu+r+\\b")
corpus$u <- str_count(corpus$content_clean_punctuation, "\\bu+\\b")
corpus$ily <- str_count(corpus$content_clean_punctuation, "\\bi+l+(y+|u+)\\b")
corpus$gtfo <- str_count(corpus$content_clean_punctuation, "\\bg+t+f+o+\\b")
corpus$sry <- str_count(corpus$content_clean_punctuation, "\\bs+r+y+\\b")
corpus$wtf <- str_count(corpus$content_clean_punctuation, "\\bw+t+f+\\b")
corpus$idk <- str_count(corpus$content_clean_punctuation, "\\bi+d+k+\\b")
corpus$omg <- str_count(corpus$content_clean_punctuation, "\\bo+m+f*g+\\b")
corpus$pls <- str_count(corpus$content_clean_punctuation, "\\bp+l+(s|z)+\\b")
corpus$ofc <- str_count(corpus$content_clean_punctuation, "\\bo+f+c+\\b")


corpus$lol <- str_count(corpus$content_clean_punctuation, "\\bl+o+l+[olz]*\\b")
corpus$lmao <- str_count(corpus$content_clean_punctuation, "\\bl+m+f*a+o+\\b")
corpus$smh <- str_count(corpus$content_clean_punctuation, "\\bs+m+f*h+\\b")
corpus$rofl <- str_count(corpus$content_clean_punctuation, "\\br+o+f+l+\\b")


corpus$inmyopinion <- str_count(corpus$content_clean_punctuation, "i+n+ m+y+( (h+o+n+e+s+t+)|(h+u+m+b+l+e+))* o+p+i+n+i+o+n+")
corpus$bytheway <- str_count(corpus$content_clean_punctuation, "b+y+ t+h+e+ w+a+y+")
corpus$tobehonest <- str_count(corpus$content_clean_punctuation, "t+o+ b+e+ h+o+n+e+s+t+")
corpus$tobefair <- str_count(corpus$content_clean_punctuation, "t+o+ b+e+ f+a+i+r+")
corpus$definitely <- str_count(corpus$content_clean_punctuation, "d+e+f+i+n+i+t+e+l+y+")
corpus$people <- str_count(corpus$content_clean_punctuation, "p+e+o+p+l+e+")
corpus$especially <- str_count(corpus$content_clean_punctuation, "e+s+p+e+c+i+a+l+l+y+")
corpus$rightnow <- str_count(corpus$content_clean_punctuation, "r+i+g+h+t+ n+o+w+")
corpus$dontworry <- str_count(corpus$content_clean_punctuation, "d+o+n+(')*t w+o+r+r+y+")
corpus$ontheotherhand <- str_count(corpus$content_clean_punctuation, "o+n+ t+h+e+ o+t+h+e+r+ h+a+n+d+")
corpus$whatthefuck <- str_count(corpus$content_clean_punctuation, "w+h+a+t+ t+h+e+ f+u+c+k+")
corpus$idontknow <- str_count(corpus$content_clean_punctuation, "i+ d+o+n+(')*t+ k+n+o+w+")
corpus$ohmygod <- str_count(corpus$content_clean_punctuation, "o+h+ m+y+ (f+u+c+k+i+n+g+ )*g+o+d+")
corpus$iknowright <- str_count(corpus$content_clean_punctuation, "i+ k+n+o+w+ r+i+g+h+t+")
corpus$shutthefuckup <- str_count(corpus$content_clean_punctuation, "s+h+u+t+ t+h+e+ f+u+c+k+ u+p+")
corpus$asfuck <- str_count(corpus$content_clean_punctuation, "a+s+ f+(u)*c+k+")
corpus$forfuckssake <- str_count(corpus$content_clean_punctuation, "f+o+r+ f+u+c+k+(')*(s)* s+a+k+e+")
corpus$your <- str_count(corpus$content_clean_punctuation, "(y+o+u+r+)|(y+o+u+(')*r+e+)")
corpus$you <- str_count(corpus$content_clean_punctuation, "\\by+o+u+\\b")
corpus$iloveyou <- str_count(corpus$content_clean_punctuation, "i+ l+o+v+e+ y+o+u+")
corpus$getthefuckout <- str_count(corpus$content_clean_punctuation, "g+e+t+ t+h+e+ f+u+c+k+ o+u+t+")
corpus$sorry <- str_count(corpus$content_clean_punctuation, "s+o+r+r+y+")
corpus$please <-str_count(corpus$content_clean_punctuation, "p+l+e+a+s+e+")
corpus$ofcourse <- str_count(corpus$content_clean_punctuation, "o+f+ c+o+u+r+s+e+")

# Descriptive statistics + inferential statistics for every acronym/abbreviation combined would be
# over 1000 lines of code. Thus, only samples are represented here.

# IMO - Non-lexicalised acronym/abbreviation - Logistic regression + Negative binomial:

# 1. IMO vs In my opinion

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

# Descriptive statistics:
imo %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)

# Logistic regression:

imomdl <- glm(shortening ~ Device, data = imo, family = "binomial")
summary(imomdl)
exp(coef(imomdl))

# Negative binomial:

imomdlnb <- glm.nb(imo ~ Device + offset(log(token_count)), data=corpus)
# check over-dispersion
odTest(imomdlnb) # Yes, over-dispersed, so continue with this model.
summary(imomdlnb)
exp(coef(imomdlnb))


# 2. TBF - Non-lexicalised acronym/abbreviation - Logistic regression + Poisson:

# TBF vs to be fair

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

# Descriptive statistics: 
summary_tbf <- tbf %>%
  group_by(Device, shortening) %>%
  summarise(shortened = n()) %>%
  mutate(total = sum(shortened),
         percent = shortened/total) %>%
  filter(shortening == "short") %>%
  dplyr::select(-shortening)
summary_tbf

# Logistic regression:

tbfmdl <- glm(shortening ~ Device, data = tbf, family = "binomial")
exp(coef(tbfmdl))
summary(tbfmdl)

# Negative binomial:

tbfmdlnb <- glm.nb(tbf ~ Device + offset(log(token_count)), data=corpus)
# Check that indeed over-dispersed:
odTest(tbfmdlnb)
# p > 0.05 thus, NOT over-dispersed. Therefore, use Poisson.
tbfmdlp <- glm(tbf ~ Device + offset(log(token_count)), family = "poisson", data=corpus)
summary(tbfmdlp)
exp(coef(tbfmdlp))

# 3. LOL - Lexicalised acronym/abbreviation - Neg. binomial:

lolmdlnb <- glm.nb(lol ~ Device + offset(log(token_count)), data=corpus)
summary(lolmdlnb)

# Check that indeed over-dispersed:
odTest(lolmdlnb)
estlol <- coef(lolmdlnb)
exp(estlol)


# 4. Frequency for device type - acronym per 100,000 tokens
# Table made for each category:

# Find out sum of tokens per device type
corpus %>% 
  group_by(Device) %>% 
  sum(token_count)
  
# Make table - this one is for lexicalised acronyms
c <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(smh,lol,lmao), sum))
longer_c <- c %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")
# Overall frequency per words
longer_c$freq_normalised <- ifelse(longer_c$Device == "Phone", longer_c$shortening_frequency * 100000 / 10897628  , 
                                   longer_c$shortening_frequency * 100000 / 3531794)

# 5. Graphs:

# Make frequency graph - this one is for lexicalised acronyms

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

# Make proportion bar graphs - this one is for the abbreviations

a <- corpus %>% 
  group_by(Device) %>% 
  summarise(across(c(ofc, pls, ppl, u,ur), sum))
longer_a <- a %>%
  pivot_longer(!Device, names_to = "shortening", values_to = "shortening_frequency")

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
