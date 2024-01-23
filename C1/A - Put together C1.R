rm(list = ls())
library(rtweet)
library(readr)
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/Twitter/C1_v2")

library(tidyverse)
library(stringr)
library(ndjson)


app_name # redacted
consumer_key # redacted
consumer_secret # redacted
client_id # redacted
client_secret # redacted
bearer_token # redacted

token <- create_token(app_name, consumer_key, consumer_secret)
auth <- rtweet_app()
auth_as(auth)


# Time to stream
streamtime <- 15 * 60
## Filename to save json data (backup)
filename <- "twitter_part_1.json"

#rt109 <- stream_tweets(timeout = streamtime, file_name = filename)

# Another way to do this:
#device <- c("Twitter for Android", "Twitter for iPhone", "Twitter Web App")
#source <- paste0("source=", device, collapse = " OR ")


rt1 <- search_tweets("lang:en", n = 20000, include_rts = FALSE)



source("https://gist.githubusercontent.com/JBGruber/dee4c44e7d38d537426f57ba1e4f84ab/raw/ce28d3e8115f9272db867158794bc710e8e28ee5/recover_stream.R")


recover_stream(file, cores = 3)
# Stream back


rt91 <- parse_stream("twitter_part_91.json")
rt92 <- parse_stream("twitter_part_92.json")
rt93 <- parse_stream("twitter_part_93.json")
rt94 <- parse_stream("twitter_part_94.json")
rt95 <- parse_stream("twitter_part_95.json") 
rt96 <- parse_stream("twitter_part_96.json")
rt97 <- parse_stream("twitter_part_97.json")
rt98 <- parse_stream("twitter_part_98.json")
rt99 <- parse_stream("twitter_part_99.json")
rt100 <- parse_stream("twitter_part_100.json")
rt86 <- parse_stream("twitter_part_86.json") 
rt87 <- parse_stream("twitter_part_87.json")
rt88 <- parse_stream("twitter_part_88.json") 
rt89 <- parse_stream("twitter_part_89.json")
rt90 <- parse_stream("twitter_part_90.json")

source("https://gist.githubusercontent.com/JBGruber/dee4c44e7d38d537426f57ba1e4f84ab/raw/ce28d3e8115f9272db867158794bc710e8e28ee5/recover_stream.R")


# Combine
df <- rbind(rt101,rt102,rt103,rt104,rt105,rt106,rt107,rt108,rt109)

df$hashtags <- as.character(df$hashtags)
df$symbols <- as.character(df$symbols)
df$urls_url <- as.character(df$urls_url)
df$urls_t.co <- as.character(df$urls_t.co)
df$urls_expanded_url <- as.character(df$urls_expanded_url)
df$ext_media_url <- as.character(df$ext_media_url)
df$media_url <- as.character(df$media_url)
df$media_t.co <- as.character(df$media_t.co)
df$media_expanded_url <- as.character(df$media_expanded_url)
df$media_type <- as.character(df$media_type)
df$ext_media_t.co <- as.character(df$ext_media_t.co)
df$ext_media_expanded_url <- as.character(df$ext_media_expanded_url)
df$mentions_user_id <- as.character(df$mentions_user_id)
df$mentions_screen_name <- as.character(df$mentions_screen_name)
df$geo_coords <- as.character(df$geo_coords)
df$coords_coords <- as.character(df$coords_coords)
df$bbox_coords <- as.character(df$bbox_coords)

write_csv2(df, "corpus_part_v1.csv") # till 89-100




corpus_part_a <- read_csv2("corpus_part_r1.csv")
corpus_part_b <- read_csv2("corpus_part_s1.csv")
corpus_part_c <- read_csv2("corpus_part_t1.csv")
corpus_part_d <- read_csv2("corpus_part_u1.csv")
corpus_part_e <- read_csv2("corpus_part_v1.csv")

corpus_part_f <- read_csv2("corpus_part_f.csv")
corpus_part_g <- read_csv2("corpus_part_g.csv")


# in 88 column: quote_count is missing and reply_count is missing
#corpus_part_w <- subset(corpus_part_w, select = -c(quote_count, reply_count))
corpus_full <- df
corpus_full <- rbind(corpus_part_a, corpus_part_b, corpus_part_c, corpus_part_d,
                     corpus_part_e)

corpus_full <- subset(corpus_full, lang %in% "en")
corpus_full <- subset(corpus_full, is_retweet == FALSE)


corpus <- subset(corpus_full, source %in% c("Twitter for Android", "Twitter for iPhone", 
                                            "Twitter Web App"))




names(corpus)[names(corpus) == "created_at"] <- "Date_Time"
names(corpus)[names(corpus) == "screen_name"] <- "Author"
names(corpus)[names(corpus) == "text"] <- "Content"
names(corpus)[names(corpus) == "source"] <- "Device"


corpus$Device <- gsub("Twitter for Android", "mobile", corpus$Device)
corpus$Device <- gsub("Twitter for iPhone", "mobile", corpus$Device)
corpus$Device <- gsub("Twitter Web App", "web", corpus$Device)
corpus$Device <- as.factor(corpus$Device)
levels(as.factor(corpus$Device))
table(corpus$Device)


# PARTS
# Part 1: 68.691 tweets. 1-15.json, a-g.csv
write_csv2(corpus, "c1.csv")
# Part 2: 105.666 tweets. 16-30.json, h-l.csv
write_csv2(corpus, "c2.csv")
# Part 3: 83.000 tweets, 31-45.json, m-q.csv
write_csv2(corpus, "c3.csv")
# Part 4: 76.838 tweets, 46-60.json, r-v.csv
write_csv2(corpus, "c4.csv")
# Part 5: 68.488 tweets, 61-75.json, w-z.csv
write_csv2(corpus, "c5.csv")
# Part 6: 120.837 tweets, 76-90.json, a1-e1.csv
write_csv2(corpus, "c6.csv")
# Part 7: 43.554 tweets, 91-101.json, f1-g1.csv
write_csv2(corpus, "c7.csv")
# Part 8: 128.094 tweets, 1-25.json, h1-l1.csv
write_csv2(corpus, "c8.csv")
# Part 9: 125.865 tweets, 26-50.json, m1-q1.csv
write_csv2(corpus, "c9.csv")
# Part 10: 165.343 tweets, 51-100.json, r1-v1.csv
write_csv2(corpus, "c10.csv")
# Part 11: 36.565 tweets, 101-109.json
write_csv2(corpus, "c11.csv")

68.691 +105.666+83.000+76.838+68.488+120.837+43.554+128.094+125.865+165.343+36.565


c1 <- read_csv2("c1.csv")
c2 <- read_csv2("c2.csv")
c3 <- read_csv2("c3.csv")
c4 <- read_csv2("c4.csv")
c5 <- read_csv2("c5.csv")
c6 <- read_csv2("c6.csv")
c7 <- read_csv2("c7.csv")
c8 <- read_csv2("c8.csv")
c9 <- read_csv2("c9.csv")
c10 <- read_csv2("c10.csv")
c11 <- read_csv2("c11.csv")


c4 <- subset(c4, select = -c(quote_count, reply_count))

corpus1 <- rbind(c1,c2,c3,c4,c5,c6)
corpus <- rbind(c7,c8,c9,c10,c11)

write_csv2(corpus, "large_twitter_corpus.csv")

corpus <- corpus[c("Date_Time", "Author", "Device", "Content", "Content_Clean", "is_quote", "is_reply",
                   "Attachments", "ext_media_url", "urls_expanded_url")]

write_csv2(corpus, "large_twitter_corpus_2.csv")



# 5. Clean up: filtration

# Clean me out
# redacted: But basically just take out if @USERNAME of Jenia was somehow a user
# in any of the columns.



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

corpus$Content <- ifelse(!is.na(corpus$Attachments), clean_urls(corpus$Content), corpus$Content)


# 8. Clean out quote links

corpus$Content <- ifelse(corpus$is_quote == TRUE, clean_urls(corpus$Content), corpus$Content)

# 9. Clean up stray spaces at end

clean_space <- function(x){
  a <- str_replace(x, "\\s+$", "")
  return(a)
}

corpus$Content <- clean_space(corpus$Content)



# Trying shit out
library(emo)
a <- corpus1 %>%
  mutate(emoji = ji_extract_all(Content_Clean)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10)
