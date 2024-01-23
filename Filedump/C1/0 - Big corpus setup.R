# 0. Corpus cleaning
library(tidyverse)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/Twitter/Big corpus attempt")

corpus <- read_csv2("large_twitter_corpus.csv")

corpus <- corpus %>%
  select(c(Author, Device, Content, reply_to_status_id, is_quote, ext_media_url ))

n_occur <- data.frame(table(corpus$Author))
n_occur[n_occur$Freq > 1,]

corpus <- corpus[corpus$Author %in% n_occur$Var1[n_occur$Freq < 11],]


corpus <- corpus[1:1000000,]

# Cleaning:

corpus$is_reply <- corpus$reply_to_status_id
corpus$is_reply[!is.na(corpus$is_reply)] <- TRUE
corpus$is_reply[is.na(corpus$is_reply)] <- FALSE
clean_users <- function(x){
  a <- str_replace(x, "^@\\w+( @\\w+)* ", "")
  return(a)
}
corpus$Content <- ifelse(corpus$is_reply == TRUE, clean_users(corpus$Content), corpus$Content)

corpus$Attachments <- ifelse(grepl("media", corpus$ext_media_url), "Picture",
                             ifelse(grepl("ext_tw_video_thumb", corpus$ext_media_url), "Video",
                                    ifelse(grepl("tweet_video_thumb", corpus$ext_media_url), "GIF", NA)))

clean_urls <- function(x){
  a <- str_replace(x, "https://t.co/\\S*$", "")
  return(a)
}
corpus$Content <- ifelse(!is.na(corpus$Attachments), clean_urls(corpus$Content), corpus$Content)
corpus$Content <- ifelse(corpus$is_quote == TRUE, clean_urls(corpus$Content), corpus$Content)
clean_space <- function(x){
  a <- str_replace(x, "\\s+$", "")
  return(a)
}
corpus$Content <- clean_space(corpus$Content)


corpus <- corpus %>%
  select(c(Author, Device, Content))
setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/LTC")
write_csv2(corpus, "twitter_big_corpus.csv")
