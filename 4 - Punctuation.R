
corpus$content_clean4 <- str_remove_all(corpus$Content, "https?://\\S*[^\\.,:;\"\'\\s]")
corpus$content_clean4 <- str_remove_all(corpus$content_clean4, "#[\\S]+")
corpus$content_clean4 <- str_remove_all(corpus$content_clean4, "@[A-z0-9_]+")
corpus$content_clean4 <- ji_replace_all(corpus$content_clean4, "")
# get emoticons from III - punctations.R
corpus$content_clean4 <- str_remove_all(corpus$content_clean4, emoticon)
# clean '
corpus$content_clean4 <- str_replace_all(corpus$content_clean4, "’", "'")


# 1. All punctuation marks.


punct1 <- "[[:punct:]]"
punct2 <- "([[:punct:]])\\1+"

corpus$punctuation_token <- str_extract_all(corpus$content_clean4, punct1)
corpus$punctuation_count <- str_count(corpus$content_clean4, punct1)

# 2. Period and comma

# Periods specifically, NOT ellipsis
corpus$period_token <- str_extract_all(corpus$content_clean4, "(?<!\\.)\\.(?!\\.+)")
corpus$period_count <- str_count(corpus$content_clean4, "(?<!\\.)\\.(?!\\.+)")

corpus$comma_token <- str_extract_all(corpus$content_clean4, ",")
corpus$comma_count <- str_count(corpus$content_clean4, ",")

# 3. Exclamation and question marks

# Single:
corpus$single_quest_token <- str_extract_all(corpus$content_clean4, "(?<!\\?)\\?(?!\\?+)")
corpus$single_quest_count <- str_count(corpus$content_clean4, "(?<!\\?)\\?(?!\\?+)")
corpus$single_excl_token <- str_extract_all(corpus$content_clean4, "(?<!\\!)\\!(?!\\!+)")
corpus$single_excl_count <- str_count(corpus$content_clean4, "(?<!\\!)\\!(?!\\!+)")

# Groups:
corpus$quest_group_token <- str_extract_all(corpus$content_clean4, "(\\?\\?)\\?*")
corpus$quest_group_count <- str_count(corpus$content_clean4, "(\\?\\?)\\?*")
corpus$excl_group_token <- str_extract_all(corpus$content_clean4, "(\\!\\!)\\!*")
corpus$excl_group_count <- str_count(corpus$content_clean4, "(\\!\\!)\\!*")


# 4. Apostrophe

corpus$apostrophe_token <- str_extract_all(corpus$content_clean4, "'")
corpus$apostrophe_count <- str_count(corpus$content_clean4, "'")
