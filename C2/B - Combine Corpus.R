# B. COMBINE CORPUS

# Clear global environment
rm(list = ls())

library(readr)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/By Week")

corpus_week_1 <- read_csv2("Corpus_Week_1_19_10_28-19_11_01.csv")
corpus_week_2 <- read_csv2("Corpus_Week_2_19_11_04-19_11_07.csv")
corpus_week_3 <- read_csv2("Corpus_Week_3_19_11_12-19_11_15.csv")
corpus_week_4 <- read_csv2("Corpus_Week_4_19_11_18-19_11_22.csv")
corpus_week_5 <- read_csv2("Corpus_Week_5_19_11_25-19_11_29.csv")
corpus_week_6 <- read_csv2("Corpus_Week_6_19_12_02-19_12_05.csv")
corpus_week_7 <- read_csv2("Corpus_Week_7_19_12_10-19_12_14.csv")
corpus_week_8 <- read_csv2("Corpus_Week_8_19_12_16-19_12_19.csv")
corpus_week_9 <- read_csv2("Corpus_Week_9_20_01_06-20_01_08.csv")
corpus_week_10 <- read_csv2("Corpus_Week_10_20_01_13-20_01_17.csv")
corpus_week_11 <- read_csv2("Corpus_Week_11_20_01_20-20_01_24.csv")
corpus_week_12 <- read_csv2("Corpus_Week_12_20_01_27_20_01_31.csv")
corpus_week_13 <- read_csv2("Corpus_Week_13_20_02_03_20_02_09.csv")
corpus_week_14 <- read_csv2("Corpus_Week_14_20_02_11_20_02_15.csv")

corpus_week_19 <- read_csv2("Corpus_Week_19_20_03_23_20_03_27.csv")
corpus_week_20 <- read_csv2("Corpus_Week_20_20_03_30_20_04_01.csv")
corpus_week_21 <- read_csv2("Corpus_Week_21_20_04_06_20_04_09.csv")
corpus_week_22 <- read_csv2("Corpus_Week_22_20_04_13_20_04_18.csv")
corpus_week_23 <- read_csv2("Corpus_Week_23_20_04_20_20_04_23.csv")
corpus_week_24 <- read_csv2("Corpus_Week_24_20_04_30_20_04_30.csv")
corpus_week_25 <- read_csv2("Corpus_Week_25_20_05_04_20_05_08.csv")
corpus_week_26 <- read_csv2("Corpus_Week_26_20_05_11_20_05_15.csv")
corpus_week_27 <- read_csv2("Corpus_Week_27_20_05_18_20_05_20.csv")
corpus_week_28 <- read_csv2("Corpus_Week_28_20_05_25_20_05_26.csv")
corpus_week_29 <- read_csv2("Corpus_Week_29_20_06_03_20_06_05.csv")
corpus_week_30 <- read_csv2("Corpus_Week_30_20_06_08_20_06_09.csv")
corpus_week_31 <- read_csv2("Corpus_Week_31_20_06_17_20_06_17.csv")
corpus_week_32 <- read_csv2("Corpus_Week_32_20_07_07_20_07_09.csv")
corpus_week_33 <- read_csv2("Corpus_Week_33_20_07_13_20_07_16.csv")
corpus_week_34 <- read_csv2("Corpus_Week_34_20_07_21_20_07_23.csv")
corpus_week_35 <- read_csv2("Corpus_Week_35_20_08_02_20_08_06.csv")
corpus_week_36 <- read_csv2("Corpus_Week_36_20_08_11_20_08_14.csv")
corpus_week_37 <- read_csv2("Corpus_Week_37_20_09_02_20_09_03.csv")
corpus_week_38 <- read_csv2("Corpus_Week_38_20_09_07_20_09_09.csv")
corpus_week_39 <- read_csv2("Corpus_Week_39_20_09_14_20_09_18.csv")
corpus_week_40 <- read_csv2("Corpus_Week_40_20_09_21_20_09_23.csv")
corpus_week_41 <- read_csv2("Corpus_Week_41_20_09_28_20_09_30.csv")




corpus <- rbind(corpus_week_1, corpus_week_2, corpus_week_3, corpus_week_4,
                corpus_week_5, corpus_week_6, corpus_week_7, corpus_week_8,
                corpus_week_9, corpus_week_10, corpus_week_11, corpus_week_12,
                corpus_week_13, corpus_week_14,
                corpus_week_19, corpus_week_20, corpus_week_21, corpus_week_22,
                corpus_week_23, corpus_week_24, corpus_week_25,
                corpus_week_26, corpus_week_27,
                corpus_week_28,
                corpus_week_29,
                corpus_week_30,
                corpus_week_31,
                corpus_week_32,
                corpus_week_33,
                corpus_week_34,
                corpus_week_35,
                corpus_week_36,
                corpus_week_37,
                corpus_week_38,
                corpus_week_39,
                corpus_week_40,
                corpus_week_41)

setwd("C:/Users/jenia/Desktop/PhD Project/2 Practical/_Corpus/Full")

write_csv2(corpus, "Corpus_ALL.csv")
