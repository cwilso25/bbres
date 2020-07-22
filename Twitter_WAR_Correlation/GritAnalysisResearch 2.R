############ WAR average data#######################
install.packages("rtweet")

library(readr)
library(rtweet)
library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)

FanGraphs_Leaderboard_Twitter <- read_csv("Downloads/FanGraphs Leaderboard Twitter.csv")

rawdata <- read.csv("Downloads/lahman_WAR_data_2019.csv")
wardata <- rawdata %>%
  select(playerID,yearID,WAR,age,PA) %>%
  mutate(playerID = as.list(playerID))

results <- list()

for(i in seq_len(nrow(FanGraphs_Leaderboard_Twitter))){
  results[[i]] <- baseballr::playername_lookup(FanGraphs_Leaderboard_Twitter$playerid[i])
}
playeridlookup <- do.call(rbind, results)

playeridlookup <- playeridlookup %>%
  select(key_fangraphs,key_bbref)

df <- cbind(FanGraphs_Leaderboard_Twitter,playeridlookup)

df <- df %>%
  select(Name,playerid,key_bbref) %>%
  mutate(key_bbref = as.list(key_bbref))

df2 <- left_join(df,wardata,c("key_bbref" = "playerID"))

##Step 3##
##Calculate average growth as the mean of the difference between two consecutive seasons
df3 <- df2 %>%
  filter(PA > 400) %>%
  group_by(Name) %>%
  mutate(Growth = (WAR - lag(WAR))) %>%
  summarize(Avg_growth = mean(Growth,na.rm = TRUE)) %>%
  mutate(Avg_growth = round(Avg_growth,3))


##eliminates Travis Jankowski, no seasons with 400 PA

Twitter_accounts <- c("@Addison_Russell","@AledmysDiaz","@BillyBurns10","@TheBrandonDrury",
                      "@OfficialBuck103","@TeamCJCorrea","@cheslorcuthbert","@coreyseager_5",
                      "@LinoDeShields","@EddieRosario09","@Lindor12BC","@JTRealmuto","@jace_petey20",
                      "@JamesMcCann34","@yungjoc650","@solerpower12","@JungHoKangGang","@bour41",
                      "@redturn2","@KrisBryant_23","@outtadapakmark","@mm_duffy","@kepleroni","@Taylor_Michael3","@NickAhmed13",
                      "NomarMzra26","@odubelherrera1","@SouzaJr","@timanderson87",
                      "@Tstory2","@eltanquetomas","@CarlosSan29")

full_dataset <- cbind(df3,Twitter_accounts) %>%
  filter(Avg_growth != 'NaN',Name != 'Aledmys Diaz') %>%
  arrange(Twitter_accounts)

##Personality Traits

Metrics <- c("You","Commas","Colons","Question Mark","Exclamation Mark",
             "Parentheses","Number of Hashtags",
             "Words per tweet","Links per tweet")
PersonalityTraits <- c("Agree, Consc","Consc","Consc","Extro","Consc,Neuro, Open",
                       "Extro,Open","Open","Open","Consc")
PosNeg <- c("P","N","P","P","P,P,N","N","P","N","P")
data.frame(cbind(Metrics,PersonalityTraits,PosNeg))

#### Twitter Data #####

## store api keys
api_key <- "3Mg1doG2HgcpPdkQonSTDCPfP"
api_secret_key <- "WTnr8WTWiaCfdRDB0XWifr7foRV3LL8zbtvwW9CvWPbSEnv5Oa"

## authenticate via web browser
token <- create_token(
  app = "GritAnalysis",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

tweets <- rtweet::get_timelines(full_dataset$Twitter_accounts,n=500)

##Find the language features## 

###You###
You <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(NumYou = sum(str_count(tolower(text),"you|your"))/n())


##Commas##
Commas <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(NumCommas = sum(str_count(text,","))/n())

##Colons##
Colons <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(NumColons = sum(str_count(text,":"))/n())

##Question Mark##
QuestionMark <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(NumQM = sum(str_count(text,"\\?"))/n())

##Exclamation Mark##
ExclamationMark <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(NumEM = sum(str_count(text,"!"))/n())

##Parentheses##
Parentheses <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(NumParentheses = sum(str_count(text,"(|)"))/n())

##Number of Hashtags##
Hashtags <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(NumHashtags = sum(str_count(text,"#"))/n())

##Words per tweet##
WordCount <- tweets %>%
  filter(is_retweet == FALSE) %>%
  select(screen_name,text) %>%
  group_by(screen_name) %>%
  summarize(WC = mean(sapply(strsplit(text," "),length))/n())

####Personality Traits##########

ds <- cbind(full_dataset,join_all(list(You,Commas,Colons,QuestionMark,ExclamationMark,
                                       Parentheses,Hashtags,WordCount),
                                                    by='screen_name', type='left')) %>%
  select(-Twitter_accounts,-screen_name)
##Correlation##
library("Hmisc")
rcorr(as.matrix(ds[2:10]), type = c("pearson"))