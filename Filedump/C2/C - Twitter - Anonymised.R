# J - TWITTER

# 1. Set up Twitter
rm(list = ls())
library(rtweet)
library(readr)
library(tidyverse)
library(stringr)
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/Twitter")

app_name # redacted
consumer_key # redacted
consumer_secret # redacted
client_id # redacted
client_secret # redacted
bearer_token # redacted

token <- create_token(app_name, consumer_key, consumer_secret)
auth <- rtweet_app()
auth_as(auth)


## save token to home directory
path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(token, path_to_token)
## create env variable TWITTER_PAT (with path to saved token)
env_var <- paste0("TWITTER_PAT=", path_to_token)
## save as .Renviron file (or append if the file already exists)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"), 
    fill = TRUE, append = TRUE)
## refresh .Renviron variables
readRenviron("~/.Renviron")

# 2. Download Twitter data

# Redacted, but here is the idea:
USER_t_corpus <- get_timeline("USERNAME", n = 5000, include_rts = FALSE)

# 3. Combine into one big thing

twitter_corpus <- rbind(jonathan_t_corpus, eliza_t_corpus, simone_t_corpus, tereza_t_corpus, roy_t_corpus,
                        alec_t_corpus, nora_t_corpus, ina_t_corpus, leila_t_corpus, michael_t_corpus, fahd_t_corpus)

# 4.
# Save ALL data (saved with commas!):
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/Twitter")
save_as_csv(twitter_corpus, "twitter_corpus_2.csv")
# Read it back:
corpus_full <- read_csv("twitter_corpus.csv")
corpus_full_2 <- read_csv("twitter_corpus_2.csv")

# 4.25 

names(corpus_full)[names(corpus_full) == "created_at"] <- "Date_Time"
names(corpus_full)[names(corpus_full) == "screen_name"] <- "Author"
names(corpus_full)[names(corpus_full) == "text"] <- "Content"


# 4.5 Split into diff ones for easier handling:
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/Twitter")

# Anonymise with gsub (see README)

jonathan <- subset(corpus_full, Author %in% "Jonathan")
eliza <- subset(corpus_full, Author %in% "Eliza")
simone <- subset(corpus_full, Author %in% "Simone")
tereza <- subset(corpus_full, Author %in% "Tereza")
roy <- subset(corpus_full, Author %in% "Roy")
alec <- subset(corpus_full, Author %in% "Alec")
nora <- subset(corpus_full, Author %in% "Nora")
ina <- subset(corpus_full, Author %in% "Ina")
leila <- subset(corpus_full, Author %in% "Leila")
michael <- subset(corpus_full, Author %in% "Michael")
fahd <- subset(corpus_full, Author %in% "Fahd")

write_csv2(jonathan, "twitter_jonathan.csv")
write_csv2(eliza, "twitter_eliza.csv")
write_csv2(simone, "twitter_simone.csv")
write_csv2(tereza, "twitter_tereza.csv")
write_csv2(roy, "twitter_roy.csv")
write_csv2(alec, "twitter_alec.csv")
write_csv2(nora, "twitter_nora.csv")
write_csv2(ina, "twitter_ina.csv")
write_csv2(leila, "twitter_leila.csv")
write_csv2(michael, "twitter_michael.csv")
write_csv2(fahd, "twitter_fahd.csv")

jonathan <- read_csv2("twitter_jonathan.csv")
eliza <- read_csv2("twitter_eliza.csv")
simone <- read_csv2("twitter_simone.csv")
tereza <- read_csv2("twitter_tereza.csv")
roy <- read_csv2("twitter_roy.csv")
alec <- read_csv2("twitter_alec.csv")
nora <- read_csv2("twitter_nora.csv")
ina <- read_csv2("twitter_ina.csv")
leila <- read_csv2("twitter_leila.csv")
michael <- read_csv2("twitter_michael.csv")
fahd <- read_csv2("twitter_fahd.csv")



corpus <- rbind(jonathan, eliza, simone, tereza, roy,
                alec, nora, ina, leila, michael, fahd)

# 5. Trim and clean data

corpus$urls_expanded_url <- as.character(corpus$urls_expanded_url)
corpus$ext_media_url <- as.character(corpus$ext_media_url)

names(corpus)[names(corpus) == "created_at"] <- "Date_Time"
names(corpus)[names(corpus) == "screen_name"] <- "Author"
names(corpus)[names(corpus) == "text"] <- "Content"
corpus$Device <- corpus$source


corpus$Device <- gsub("Twitter for Android", "mobile", corpus$Device)
corpus$Device <- gsub("Twitter for iPhone", "mobile", corpus$Device)
corpus$Device <- gsub("Twitter Web App", "web", corpus$Device)
corpus$Device <- gsub("Twitter Web Client", "web", corpus$Device)
corpus$Device <- gsub("TweetDeck", "web", corpus$Device)
corpus$Device <- as.factor(corpus$Device)
levels(as.factor(corpus$Device))

# Fix device:

#corpus$source <- as.factor(corpus$source)
#levels(as.factor(corpus$source))

#corpus <- subset(corpus, source %in% c("Twitter for Android", "Twitter for iPhone", "Twitter Web App", "Twitter Web Client",
               #                        "TweetDeck"))
#corpus$Device <- corpus$source

# Anonymise people:
# ibid

# Check
table(corpus$Author, corpus$Device)

corpus_mw <- subset(corpus, Device %in% c("mobile", "web"))
table(corpus_mw$Author, corpus_mw$Device)
corpus <- corpus_mw
corpus$Device <- factor(corpus$Device, levels = c("mobile", "web"))

# 5. Clean up: filtration

# Clean me out

corpus <- filter(corpus, !grepl("[my username]", mentions_screen_name))
corpus <- filter(corpus, !grepl("[my username]", quoted_screen_name))
#grepl("[my username]", corpus$mentions_screen_name)

table(corpus_3$Author, corpus_3$Device)



# 6. Clean out @users

corpus$is_reply <- corpus$reply_to_status_id
corpus$is_reply[!is.na(corpus$is_reply)] <- TRUE
corpus$is_reply[is.na(corpus$is_reply)] <- FALSE

clean_users <- function(x){
  a <- str_replace(x, "^@\\w+( @\\w+)* ", "")
  return(a)
}

corpus$Content_Clean <- ifelse(corpus$is_reply == TRUE, clean_users(corpus$Content), corpus$Content)

# 7. Clean out media

corpus$Attachments <- ifelse(grepl("media", corpus$ext_media_url), "Picture",
                             ifelse(grepl("ext_tw_video_thumb", corpus$ext_media_url), "Video",
                                    ifelse(grepl("tweet_video_thumb", corpus$ext_media_url), "GIF", NA)))

clean_urls <- function(x){
  a <- str_replace(x, "https://t.co/\\S*$", "")
  return(a)
}

corpus$Content_Clean <- ifelse(!is.na(corpus$Attachments), clean_urls(corpus$Content_Clean), corpus$Content_Clean)


# 8. Clean out quote links

corpus$Content_Clean <- ifelse(corpus$is_quote == TRUE, clean_urls(corpus$Content_Clean), corpus$Content_Clean)

# 9. Clean up stray spaces at end

clean_space <- function(x){
  a <- str_replace(x, "\\s+$", "")
  return(a)
}

corpus$Content_Clean <- clean_space(corpus$Content_Clean)


# 10. Write full corpus
write_csv2(corpus, "twitter_corpus_clean_all_columns.csv")

# 11. What columns make up corpus_clean?
corpus <- corpus[c("Date_Time", "Author", "Device", "Content_Clean", "is_quote", "is_reply",
                   "Attachments", "ext_media_url", "urls_expanded_url")]

# 12. Conversational vs broadcast tweets
# As in pattern:
corpus_full$mentions_screen_name <- gsub("[USERNAME]", "Jonathan", corpus_full$mentions_screen_name)

# As in pattern:
corpus_full$quoted_screen_name <- gsub("[USERNAME]", "Jonathan", corpus_full$quoted_screen_name)


corpus_convo <- corpus_full[c("Author", "is_quote", "is_reply", "mentions_screen_name", "quoted_screen_name")]


corpus_convo$standalone <- ifelse((corpus_convo$is_quote == TRUE & corpus_convo$quoted_screen_name != corpus$Author) |
                                    (corpus_convo$is_reply == TRUE & !is.na(corpus_convo$mentions_screen_name)),
                                  "conversational", "broadcast")
corpus$standalone <- corpus_convo$standalone


# 7. Write it:

write_csv2(corpus, "twitter_corpus_cleaned.csv")
corpus <- read_csv2("twitter_corpus_cleaned.csv")

