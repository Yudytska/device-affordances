# Graphicons.


corpus$content_clean3 <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean3 <- str_remove_all(corpus$content_clean3, "#[\\S]+")
corpus$content_clean3 <- str_remove_all(corpus$content_clean3, "@[A-z0-9_]+")


# 1. Emojis

corpus$emoji_count <- ji_count(corpus$Content)
corpus$emoji_token <- ji_extract_all(corpus$Content)


e1 <- "\\)+"
e2 <- "\\(+"
e3 <- "\\|+"
e5 <- "o+"
e6 <- "d+"
e7 <- "p+"
e8 <- "0+"
e100 <- paste0(c(e1,e2,e3,e5,e6,e7,e8), collapse="|")
emoticon1 <- paste0(c("(?<![0-9])(&gt;)*[=:;](')*(\\-)*", "(", e100, ")", "(?![0-9A-z])"), collapse="")
emoticon2 <- "(?<![0-9A-z])xd+(?![0-9A-z])"
emoticon3 <- "(?<![0-9A-z])[0o\\*~][._^]+[0o\\*~][;']*(?![0-9A-z])"
emoticon4 <- "(?<![0-9A-z])\\^[._]*\\^(?![0-9A-z])"
emoticon5 <- "&lt;(\\/)*3+"
emoticon <- paste0(c(emoticon1, emoticon2, emoticon3, emoticon4, emoticon5), collapse = "|")

corpus$emoticon_token <- str_extract_all(corpus$content_clean3, emoticon)
corpus$emoticon_count <- str_count(corpus$content_clean3, emoticon)
