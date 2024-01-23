# A. SETTING UP DATA

# 1. Libraries and working directory

# Clear global environment
rm(list = ls())

library(readr)
library(readxl)
library(writexl)
library(lubridate)

# yyyy-MM-dd HH:mm:ss TIME SETTING

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Chat Exports")

# 2. Import data from all channels

general <- read_csv("BlogDiscord - general [552822891394105354] (2019-12-09 to 2019-12-15).csv")
general$Channel <- "general"

sensible_chat <- read_csv("BlogDiscord - sensible-chat [618781054370709534] (2019-12-09 to 2019-12-15).csv")
sensible_chat$Channel <- "sensible-chat"

the_tome_zone <- read_csv("BlogDiscord - the-tome-zone [552822653765943306] (2019-12-09 to 2019-12-15).csv")
the_tome_zone$Channel <- "the-tome-zone"

spoilers <- read_csv("BlogDiscord - spoilers [552822594265415700] (2019-12-09 to 2019-12-15).csv" )
spoilers$Channel <- "spoilers"

talk_it_out <- read_csv("BlogDiscord - talk-it-out [552822121470885888] (2019-12-09 to 2019-12-15).csv" )
talk_it_out$Channel <- "talk-it-out"

shout_it_out <- read_csv("BlogDiscord - shout-it-out [552822052994809857] (2019-12-09 to 2019-12-15).csv")
shout_it_out$Channel <- "shout-it-out"

hall_of_shame <- read_csv("BlogDiscord - hall-of-shame [478519735852728320] (2019-12-09 to 2019-12-15).csv")
hall_of_shame$Channel <- "hall-of-shame"

comics <- read_csv("BlogDiscord - comics [555017371589279754] (2019-12-09 to 2019-12-15).csv")
comics$Channel <- "comics"

cute_shit <- read_csv("BlogDiscord - cute-shit [552822309203869726] (2019-12-09 to 2019-12-15).csv")
cute_shit$Channel <- "cute-shit"

games <- read_csv("BlogDiscord - games [552822358709370893] (2019-12-09 to 2019-12-15).csv")
games$Channel <- "games"

hawties <- read_csv("BlogDiscord - hawties [624682823298842624] (2019-12-09 to 2019-12-15).csv")
hawties$Channel <- "hawties"

podcasts <- read_csv("BlogDiscord - podcasts [555018290947424257] (2019-12-09 to 2019-12-15).csv")
podcasts$Channel <- "podcasts"

tv_and_movies <- read_csv("BlogDiscord - tv-and-movies [552822414900461578] (2019-12-09 to 2019-12-15).csv")
tv_and_movies$Channel <- "tv-and-movies"

writing <- read_csv("BlogDiscord - writing [552821427837992960] (2019-12-09 to 2019-12-15).csv")
writing$Channel <- "writing"

software_shit <- read_csv("BlogDiscord - software-shit [589925941653012481] (2019-12-09 to 2019-12-15).csv")
software_shit$Channel <- "software-shit"

art <- read_csv("BlogDiscord - art [514937981908287488] (2019-12-09 to 2019-12-15).csv")
art$Channel <- "art"

buddy_reads <- read_csv("BlogDiscord - buddy-reads [552821195003658240] (2019-12-09 to 2019-12-15).csv")
buddy_reads$Channel <- "buddy-reads"

# 3. Combine data

corpus_all <- rbind(general, sensible_chat, the_tome_zone, spoilers,
                    talk_it_out, shout_it_out, hall_of_shame, comics,
                    cute_shit, games, hawties, podcasts, tv_and_movies,
                    writing, software_shit, art, buddy_reads)

# 4a. Convert time to something workable

# Old format:
# cleantime <- function(x){
#   ct <- as.POSIXct(x,  tz = "CET", format="%d-%b-%y %I:%M %p")
#   return(ct)
# }

# Apparently not necessary anymore since the last udpate????
# cleantime <- function(x){
#    ct <- as.POSIXct(x,  tz = "CET", format="%Y-%m-%d %H:%M:%OS")
#    return(ct)
#  }

# corpus_all$Date_Time <- cleantime(corpus_all$Date)

names(corpus_all)[names(corpus_all) == "Date"] <- "Date_Time"


# 4b. As writing to Excel converts everything to UCT, have to add another
#     column, which is the CET time, for easier parsing
#     Apparently not the case anymore?? But still easier parsing as char

corpus_all$Time <- as.character(corpus_all$Date_Time)


# 5. Extract only comments by participants and time

# Get all intervals from schedule excel file


int1 <- interval("2019-12-10 16:42:00", "2019-12-10 21:01:00")
int2 <- interval("2019-12-11 15:41:00", "2019-12-11 17:35:00")
int3 <- interval("2019-12-14 11:56:00", "2019-12-14 20:13:00")
int4 <- interval("2019-12-04 11:54:00", "2019-12-04 14:09:00")
int5 <- interval("2019-12-04 14:30:00", "2019-12-04 18:17:00")
int6 <- interval("2019-12-05 09:52:00", "2019-12-05 16:30:00")
int7 <- interval("2019-11-28 13:45:00", "2019-11-28 16:00:00")
int8 <- interval("2019-11-28 16:31:00", "2019-11-28 18:18:00")
int9 <- interval("2019-11-29 10:20:00", "2019-11-29 12:01:00")
int10 <- interval("2019-11-29 14:41:00", "2019-11-29 16:45:00")
int11 <- interval("2019-11-15 13:26:00", "2019-11-15 16:17:00")

corpus <- subset(corpus_all, Author %in% c("333026706912837634", 
                                             "274687431855308801", 
                                             "475258241740177421", 
                                             "71967059436638208", 
                                             "306376635987525632",
                                             "91645511487979520", 
                                             "306587574900752384",
                                             "409765567759777793",
                                             "330792506482753577",
                                             "231601535354667010",
                                             "129363764783874048")) &
                   (Date_Time %within% int1 | Date_Time %within% int2
                    | Date_Time %within% int3 | Date_Time %within% int4
                    | Date_Time %within% int5 | Date_Time %within% int6)
                   )

# 6. Export part of corpus as Excel file to add Device column

corpusexcel <- corpus[,c("Date_Time", "Author", "Content", "Channel")]
# Add extra ID column for easier organising
corpusexcel$ID <- 1:nrow(corpus)

write_xlsx(corpusexcel, "_Corpus_Excel.xlsx")




# 7. Add Device column ONLY to rest of data [which is still kept in
# CSV + UCF-8 encoding]

# DON'T FORGET TO ORDER BACK BY ID!!!

corpusdevice <- read_xlsx("_Corpus_Excel.xlsx")

corpus <- cbind(corpus, Device = corpusdevice$Device)

# 8. Reorder columns

corpus <- corpus[c("Date_Time", "Channel", "Author", "Device", "Content", "Attachments", "Reactions")]

#9. Add edited columns
# Unfortunately, coding has to be done by hand atm.
# Find all HTML files to get:
unique(as.factor(corpus$Channel))

# Add EditedComment column
corpus$EditedComment <- NA

# Go to HTML file and ctrl+f "edited"
# e.g.
grep("Dying to say something sarcastic", corpus$Content)

# This gives the number of the row ("x")
# corpus[x,"EditedComment"] <- "Edited"
corpus[422,"EditedComment"] <- "Edited"

# 10. Save as file and move file to corpus area

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/By Week")

write_csv2(corpus, "Corpus_Week_7_19_12_10_19_12_14.csv")


