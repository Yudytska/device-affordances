# Shortenings

corpus <- corpus[,1:11]

lotr <- "lotr"
asoiaf <- "asoiaf"
hp <- "\\bhp\\b"
kkc <- "kkc"
wrts <- "wrts"
sff <- "sff"
ya <- "\\bya\\b"
rec <- "\\brec(s)*\\b"
reccing <- "rec(c)*ing"
recced <- "recced"
tbr <- "\\btbr[sd]*\\b"
dnf <- "dnf"
arc <- "\\barc(s)*\\b"
gr <- "\\bgr\\b"
uf <- "\\buf\\b"
hea <- "\\bhea\\b"
pnr <- "\\bpnr\\b"
mc <- "\\bmc(s)*\\b"
wip <- "wip"


lordoftherings <- "lord of the rings"
asongoficeandfire <- "a song of ice and fire"
harrypotter <- "harry potter"
wheeloftime <- "wheel of time"
kingkiller <- "king(\\s)*killer"
got <- "\\bgot\\b"
wot <- "wot"
gameofthrones <- "game of thrones"
storm <- "\\bstorm\\b"


youngadult <- "young adult"
recommendation <- "recommend"
toberead <- "to be read"
didnotfinish <- "did not finish"
advance <- "(?<!in )advance(?!d)"
galley <- "\\bgalley\\b"
goodreads <- "goodreads"
urbanfantasy <- "urban fantasy"
happilyeverafter <- "happily ever after"
paranormalromance <- "paranormal romance"
maincharacter <- "main character"
workinprogress <- "work.in.progress"

# GOT and WOT - take out for now
# Where should "galley" go?
book_acronyms <- paste0(c(lotr, asoiaf, hp, kkc, wrts), collapse = "|")
book_full <- paste0(c(lordoftherings, asongoficeandfire, harrypotter, wheeloftime, kingkiller,
                storm), collapse = "|")
comm_acronyms <- paste0(c(sff, ya, rec, recced, reccing, tbr, dnf, arc,  gr, uf, hea, pnr,
                   mc, wip), collapse = "|")
comm_full <- paste0(c(youngadult, recommendation, toberead, didnotfinish, advance, goodreads,
               urbanfantasy, happilyeverafter, paranormalromance, maincharacter,
               workinprogress), collapse = "|")


corpus$book_acronyms <- str_count(corpus$content_clean, book_acronyms)
corpus$book_full <- str_count(corpus$content_clean, book_full)
corpus$comm_acronyms <- str_count(corpus$content_clean, comm_acronyms)
corpus$comm_full <- str_count(corpus$content_clean, comm_full)

corpus$book_acronyms[is.na(corpus$book_acronyms)] <- 0
corpus$book_full[is.na(corpus$book_full)] <- 0
corpus$comm_acronyms[is.na(corpus$comm_acronyms)] <- 0
corpus$comm_full[is.na(corpus$comm_full)] <- 0

comm_acronym <- corpus %>%
  group_by(Participant, Channel, Type) %>%
  summarise(comm_acronym_freq = sum(comm_acronyms),
            word_count = sum(word_count))
comm_acronym$freq_normalised <- comm_acronym$comm_acronym_freq * 1000 / comm_acronym$word_count 
comm_acronym

ggplot(comm_acronym, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "Community acronym frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# Book acronyms are so rare on Discord, there's really nothing to be said.
book_acronym <- corpus %>%
  group_by(Participant, Channel, Type) %>%
  summarise(book_acronym_freq = sum(book_acronyms),
            word_count = sum(word_count))
book_acronym$freq_normalised <- book_acronym$book_acronym_freq * 1000 / book_acronym$word_count 
book_acronym

ggplot(book_acronym, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "Book acronym frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Common acronyms:

corpus$content_clean <- tolower(corpus$content_clean)
corpus$imo <- str_count(corpus$content_clean, "\\bi+m+h*o+\\b")
corpus$btw <- str_count(corpus$content_clean, "\\bb+t+w+\\b")
corpus$tbh <- str_count(corpus$content_clean, "\\bt+b+h+\\b")
corpus$tbf <- str_count(corpus$content_clean, "\\bt+b+f+\\b")
corpus$rn <- str_count(corpus$content_clean, "\\br+n+\\b")
corpus$af <- str_count(corpus$content_clean, "\\ba+f+\\b")
corpus$ffs <- str_count(corpus$content_clean, "\\bf+f+s+\\b")
corpus$wtf <- str_count(corpus$content_clean, "\\bw+t+f+\\b")
corpus$idk <- str_count(corpus$content_clean, "\\bi+d+k+\\b")
corpus$omg <- str_count(corpus$content_clean, "\\bo+m+f*g+\\b")


corpus <- corpus %>% 
  rowwise() %>%
  mutate(acronyms = sum(c_across(imo:omg)))


acronym <- corpus %>%
  group_by(Participant, Channel, Type) %>%
  summarise(acronym_freq = sum(acronyms, na.rm = TRUE),
            word_count = sum(word_count))
acronym$freq_normalised <- acronym$acronym_freq * 1000 / acronym$word_count 
acronym

ggplot(acronym, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "Acronym frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))


# Abbreviations:
# There's so little of it, I don't kow if it's worth doing.
corpus$def <- str_count(corpus$content_clean, "\\bd+e+f+\\b")
corpus$u <- str_count(corpus$content_clean, "\\bu+\\b")
corpus$ur <- str_count(corpus$content_clean, "\\bu+r+\\b")
corpus$esp <- str_count(corpus$content_clean, "\\besp\\b")
corpus$v <- str_count(corpus$content_clean, "\\bv\\b")


corpus <- corpus %>% 
  rowwise() %>%
  mutate(abbreviations = sum(c_across(def:v)))


abbreviations <- corpus %>%
  group_by(Participant, Channel, Type) %>%
  summarise(abbreviation_freq = sum(abbreviations, na.rm = TRUE),
            word_count = sum(word_count))
abbreviations$freq_normalised <- abbreviations$abbreviation_freq * 1000 / abbreviations$word_count 
abbreviations

ggplot(abbreviations, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "Abbreviation frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))



corpus$lol <- str_count(corpus$content_clean, "\\bl+o+l+[olz]*\\b")
corpus$smh <- str_count(corpus$content_clean, "\\bs+m+f*h+\\b")


corpus <- corpus %>% 
  rowwise() %>%
  mutate(acronyms = sum(c_across(lol:smh)))


comm_acronym <- corpus %>%
  group_by(Participant, Channel, Type) %>%
  summarise(comm_acronym_freq = sum(acronyms, na.rm = TRUE),
            word_count = sum(word_count))
comm_acronym$freq_normalised <- comm_acronym$comm_acronym_freq * 1000 / comm_acronym$word_count 
comm_acronym

ggplot(comm_acronym, aes(y=freq_normalised, x=Participant, fill = Participant)) + 
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap("Channel", scales = "free") +
  theme(legend.position="none") +
  theme_clean(base_size = 15) +
  scale_fill_manual(values = my_colors2) +
  labs(x="Participants over channels",
       y = "LOL + SMH frequency (per 1,000 tokens)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size=0.5, colour = "black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

# To examine overall how frequent acronyms are: find columns, then do a colSums on them
acronyms <- corpus[,12:14]
colSums(acronyms, na.rm = TRUE)
