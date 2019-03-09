### There are three types of tweets 'Relief_Tracker' sends out.

### Type 1: The Awareness Tweet
#### These disasters are unknown to most people in the U.S.
#### These tweets are designed to inform them of the disaster on a basic level.
#### These could start a conversation or spark interest to those who want to learn more.

### Type 2: The Engagement Tweet
#### These disasters still have avenues for people to donate.
#### They will serve as ways for people to contribute to the recovery.

### Type 3: The Focus Tweet
#### These are disasters that had our attention, lost it and are still recovering.
#### They will provide updates on the recovery and try to keep it in the news cycle.


#Code below

setwd("/Users/juliansaliani/Desktop/Misc.(Random)/Tracker_Code/")


#Load Libraries

library(rvest)
library(stringr)
library(dplyr)
library(twitteR)

api_keys <- read.csv("Hurricane_Twitter_Account.csv",header = FALSE,stringsAsFactors = FALSE)


# Set up twitter api

setup_twitter_oauth(consumer_key = api_keys[1,2],
                    consumer_secret = api_keys[2,2],
                    access_token = api_keys[3,2],
                    access_secret = api_keys[4,2])



time <- Sys.time()



###############
#Data Set up
###############

#Pull in basic data tables
Metadata <- read.csv('Metadata_Chart.csv')
Started <- read.csv('Started_Type.csv')


#Pull in tweet tables
FinalInput <- cbind(as.matrix(read.delim("Final_.txt")),1)

################################
#Phase 1: Days been Ind Tweet and create loop for all disasters.
################################

#create Preface matrix
Preface <- vector(length = 2)
Preface[1] <- "The "
Preface[2] <- ""


#Put together the first half of the tweet, which includes data hashtag and date

PhraseMatrix <- matrix(ncol=6,nrow=dim(Metadata)[1])
for(i in 1:(dim(Metadata)[1])){
  Phrase <- vector("list",length=4)
  day <- as.numeric(as.Date(Sys.Date()) - as.Date(Metadata[i,4],"%m/%d/%y"))
  PhraseMatrix[i,1] <- as.character(Metadata[i,6])
  PhraseMatrix[i,2] <- paste0(Started[Metadata[i,5],2])
  PhraseMatrix[i,3] <- i
  PhraseMatrix[i,4] <- day
  PhraseMatrix[i,5] <- as.character(Metadata[i,4])
  PhraseMatrix[i,6] <- paste0(Preface[Metadata[i,7]])
}

colnames(PhraseMatrix) <- c("Disaster Hashtag","Status","Disaster #","Days since","Date started")

##################################
#Phase 2: Edit hand curated tweets
##################################


#Substitute ")n" with "\n": fixes \n problem in excel

for(i in 1:dim(FinalInput)[1]){
  FinalInput[i,3] <- gsub(")n","\n",FinalInput[i,3])
}


#Create Combined Table

Combined <- FinalInput

Combined <- Combined[order(as.numeric(Combined[,2])),]

#eliminate spaces in 2nd & 4th column
for(i in 1:dim(Combined)[1]){
  Combined[i,2] <- gsub("[ ]","",Combined[i,2])
  Combined[i,4] <- gsub("[ ]","",Combined[i,4])
  Combined[i,5] <- gsub("[ ]","",Combined[i,5])
}

if((as.numeric(Sys.Date()) %% 2 == 0) == FALSE){
  Combined <- Combined[which(Combined[,4] == 1),]
}else{
  Combined <- Combined[which(Combined[,4] == 2),]
}
  
#################################
#Step 3: Create Tweet to send out
#################################

#Create Tweet matrix that holds all necessary data
TweetMatrix <- matrix(ncol=3,nrow=dim(Metadata)[1])

#####Format
#Disaster: #input
#Started On: 10/01/2016 (760 days ago)
#___Text____
#-Info #1

for(i in 1:dim(Metadata)[1]){
  CombinedTemp <- Combined[which(Combined[,2] == i),]
  if(is.null(dim(CombinedTemp)[1]) == FALSE){
    CombinedTemp <- CombinedTemp[round(runif(1,1,dim(CombinedTemp)[1]),0),]
  }
  if(Metadata[i,8] == 1){
    TweetMatrix[i,1] <- paste0(PhraseMatrix[i,1],"\n")
  }else{
  TweetMatrix[i,1] <- paste0(PhraseMatrix[i,6],PhraseMatrix[i,1]," ",PhraseMatrix[i,2]," ",PhraseMatrix[i,4]," days ago.")
  }
  TweetMatrix[i,2] <- CombinedTemp[3]
  TweetMatrix[i,3] <- CombinedTemp[5]
}


#Combine into one vector with each Tweet to send out.
TweetThread <- vector(length=dim(TweetMatrix)[1])
TweetPerDay <- TweetMatrix[,3]

for(i in 1:length(TweetThread)){
  TweetThread[i] <- paste0(TweetMatrix[i,1],TweetMatrix[i,2])
}

#Order by release date
FinalTweetThread <- TweetThread[order(as.numeric(PhraseMatrix[,4]))]

#Check for NA's
if(is.numeric(which(is.na(TweetMatrix[,2]) == TRUE)) == TRUE){
  FinalTweetThread[-which(is.na(TweetMatrix[,2]) == TRUE)]
}

##### SAFETY PROOF MEASURE FOR NOT DUPLICATING TWEETS!!!!!!
e <- 0
f <- 0
h <- 0


#################################
#Step 4: Tweet'em out!!!
#################################


#Back to the threading based off the initial tweet.
#Need to add extra barrier
#Tweets before 12!!!!

#if(substring(format(Sys.time(),"%X"),1,2) <=12 & e == 0){

if(e == 0){
  ExportTweetThread <- FinalTweetThread[which(TweetPerDay == 1)]
for(j in 1:length(ExportTweetThread)){
  g <- updateStatus(ExportTweetThread[j])
  Sys.sleep(120)
}
e <- 1}

#If you ever have character limit trouble, follow this
updateStatus <- edit(updateStatus)
##Change 140 to 280



