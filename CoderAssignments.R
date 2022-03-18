
tweets <-read.csv("/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/Tweets_Raw_9.17.csv")



accts <- unique(tweets$Username)
accts <- as.data.frame(accts)
write.csv(accts, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/accts.csv")




Aliyah1 <- read.csv("/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/Aliyah_Tweets_Sheet1.csv")

Aliyah2 <- read.csv("/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/Aliyah_Tweets_Sheet1.csv")

library(tidyverse)

#merge all data frames together
Aliyah <- rbind(Aliyah1, Aliyah2)

Aliyah <- filter(Aliyah, Coder_Initials == 'AMC')

# number of unique claims:
length(unique(Aliyah$Tweet.Id)) ## only 131 unique claims 3/4

# Aliyah's Coded Tweets:
AliyahcodedDone <- unique(Aliyah$Tweet.Id)

AliyahComb = merge(x=tweets, y = Aliyah, by ="Tweet.Id", all.x  = TRUE) 


## halijeuleah! this is the remainder
tweetsUncoded = tweets %>% anti_join(Aliyah, by = "Tweet.Id")

# Reformat datetime into date and time
tweetsUncoded$Date <- as.Date(tweetsUncoded$Datetime) # already got this one from the answers above
tweetsUncoded$Time <- format(as.POSIXct(tweetsUncoded$Datetime), format = "%H:%M:%S") 

# Subset Tweets remaining by date:
tweetsRemain <- subset(tweetsUncoded, Date > "2020-05-24" & Date < "2020-06-14")

# filter duplicates
dupes <- tweetsRemain %>% 
  group_by(Tweet.Id) %>% 
  filter(duplicated(Tweet.Id))

tweetsRemain2 = tweetsRemain %>% anti_join(dupes, by = "Tweet.Id")
tweetsRemain = merge(x=tweetsRemain2, y = dupes, by ="Tweet.Id", all = TRUE) 
tweetsRemain$X <- tweetsRemain$X.x

colnames(tweetsRemain) <- c("Tweet.Id", "X", "Datetime", "Text", "Username", 
                            "Date", "Time")
keeps <- c("Tweet.Id", "X", "Datetime", "Text", "Username", 
           "Date", "Time")
tweetsRemain = tweetsRemain[keeps]

# assign tweeets to Coders:
AliyahWeek3 <- subset(tweetsRemain, X <= 900)
write.csv(AliyahWeek3, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/AliyahWeek3.csv")

LucyWeek1 <- subset(tweetsRemain,  X > 900 & X < 1900)
write.csv(LucyWeek1, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/LucyWeek1.csv")


AricaWeek1 <- subset(tweetsRemain, X > 1900 & X < 5000)
write.csv(AricaWeek1, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/AricaWeek1.csv")


AliyahWeek4 <- subset(tweetsRemain, X > 5000 & X < 6000)
write.csv(AliyahWeek4, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/AliyahWeek4.csv")

LucyWeek2 <- subset(tweetsRemain, X > 6000 & X < 7700)
write.csv(LucyWeek2, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/LucyWeek2.csv")

AricaWeek2 <- subset(tweetsRemain, X > 7700 & X < 8000)
write.csv(AricaWeek2, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/AricaWeek2.csv")

AliyahWeek5 <- subset(tweetsRemain, X >8000 & X < 8300)
write.csv(AliyahWeek5, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/AliyahWeek5.csv")

LucyWeek3 <- subset(tweetsRemain, X > 8300 & X < 8500)
write.csv(LucyWeek3, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/LucyWeek3.csv")

AliyahWeek6 <- subset(tweetsRemain, X >8500 & X < 8700)
write.csv(AliyahWeek6, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/AliyahWeek6.csv")

LucyWeek4 <- subset(tweetsRemain, X > 8700 & X < 10000)
write.csv(LucyWeek4, "/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/LucyWeek4.csv")

RemainApril <- subset(tweetsRemain, X > 10000)
