# 

corpus$content_clean <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean <- str_remove_all(corpus$content_clean, "#[\\S]+")
corpus$content_clean <- str_remove_all(corpus$content_clean, "@[A-z0-9_]+")
corpus$content_clean <- ji_replace_all(corpus$content_clean, "!")
# get emoticons from III - Emojis.R
corpus$content_clean <- str_replace_all(tolower(corpus$content_clean), all_emoticon, "!")

# All-capitalised messages
corpus$allcapscomment_count <- str_detect(corpus$content_clean, "[a-z]", negate = TRUE)

# All-lowercase messages
corpus$alllowcomment_count <- str_detect(corpus$content_clean, "[A-Z]", negate = TRUE)

# First letter capitalisation
firstcapitalletter <- function(x){
  fcl <- str_detect(x, "^[A-Z][^[A-Z]]")
  return(fcl)
}
firstlowercase <- function(x){
  flc <- str_detect(x, "^[a-z]")
  return(flc)
}
clean_urls <- function(x){
  a <- str_replace(x, "https", "URL")
  return(a)
}

corpus$Content <- clean_urls(corpus$Content)
corpus$Capital <- firstcapitalletter(corpus$Content)
corpus$Lowercase <- firstlowercase(corpus$Content)
corpus$Firstletter <- ifelse(corpus$Capital == TRUE, "Uppercase", 
                             ifelse(corpus$Lowercase == TRUE, "Lowercase", NA))

subcorpus <- subset(corpus, !is.na(Firstletter))
subcorpus$Firstletter <- factor(subcorpus$Firstletter, levels = c("Uppercase", "Lowercase"))


# Letter iteration

corpus$letter_repeat <- str_extract_all(corpus$content_clean, "\\b(?=\\w*([a-z])\\1\\1)\\w+\\b") # At least three
corpus$letter_repeat_count <- str_count(corpus$content_clean, "\\b(?=\\w*([a-z])\\1\\1)\\w+\\b")
