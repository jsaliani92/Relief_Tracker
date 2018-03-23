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


##################
#Code Starts Here#
##################

#set your directory
setwd("_______Insert Directory________")


#Load Libraries

library(rvest)
library(stringr)
library(dplyr)
library(twitteR)

#pull in api_keys
api_keys <- read.csv("__________Insert API Code Matrix___________",header = FALSE,stringsAsFactors = FALSE)


# Set up twitter api
setup_twitter_oauth(consumer_key = api_keys[1,2],
                    consumer_secret = api_keys[2,2],
                    access_token = api_keys[3,2],
                    access_secret = api_keys[4,2])



time <- Sys.time()


#Edit twitter function to include 280 characters
updateStatus <- edit(updateStatus)
##Change 140 to 280


###############
#Data Set up
###############

#Pull in basic data tables
Metadata <- read.csv('Metadata_Chart.csv')
Started <- read.csv('Started_Type.csv')

#Pull in tweet tables
Awareness <- cbind(as.matrix(read.delim("Awareness_new.txt")),1)
Act <- cbind(as.matrix(read.delim("Act_new.txt")),2)
Focus <- cbind(as.matrix(read.delim("Focus_new.txt")),3)

# Remove NA's
Awareness <- Awareness[,-which(is.na(Awareness[1,])==TRUE)]
Awareness <- Awareness[-which(is.na(Awareness[,2])==TRUE),]
Focus <- Focus[,-which(is.na(Focus[1,])==TRUE)]

################################
#Phase 1: Days been Ind Tweet and create loop for all disasters.
################################


#Put together the first half of the tweet, which includes data hashtag and date

PhraseMatrix <- matrix(ncol=5,nrow=dim(Metadata)[1])
for(i in 1:(dim(Metadata)[1])){
  Phrase <- vector("list",length=4)
  day <- as.numeric(as.Date(Sys.Date()) - as.Date(Metadata[i,4],"%m/%d/%y"))
  PhraseMatrix[i,1] <- as.character(Metadata[i,6])
  PhraseMatrix[i,2] <- paste0(Started[Metadata[i,5],2])
  PhraseMatrix[i,3] <- i
  PhraseMatrix[i,4] <- day
  PhraseMatrix[i,5] <- as.character(Metadata[i,4])
}

colnames(PhraseMatrix) <- c("Disaster Hashtag","Status","Disaster #","Days since","Date started")


##################################
#Phase 2: Edit hand curated tweets
##################################


#Substitute ")n" with "\n": fixes \n problem in excel

for(i in 1:dim(Act)[1]){
  Act[i,3] <- gsub(")n","\n",Act[i,3])
}

for(i in 1:dim(Awareness)[1]){
  Awareness[i,3] <- gsub(")n","\n",Awareness[i,3])
}


for(i in 1:dim(Focus)[1]){
  Focus[i,3] <- gsub(")n","\n",Focus[i,3])
}

#Create Combined Table

Combined <- rbind(Awareness,Act,Focus)

Combined <- Combined[order(as.numeric(Combined[,2])),]

#eliminate spaces in 2nd column
for(i in 1:dim(Combined)[1]){
  Combined[i,2] <- gsub("[ ]","",Combined[i,2])
}

#################################
#Step 3: Create Tweet to send out
#################################

#Create Tweet matrix that holds all necessary data
TweetMatrix <- matrix(ncol=2,nrow=dim(Metadata)[1])

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
  TweetMatrix[i,1] <- paste0("Disaster: ",PhraseMatrix[i,1],"\n",PhraseMatrix[i,2]," ",PhraseMatrix[i,4]," days ago")
  TweetMatrix[i,2] <- CombinedTemp[3]
}

#Combine into one vector with each Tweet to send out.
TweetThread <- vector(length=dim(TweetMatrix)[1])


for(i in 1:length(TweetThread)){
  TweetThread[i] <- paste0(TweetMatrix[i,1],TweetMatrix[i,2])
}

#Order by release date
FinalTweetThread <- TweetThread[order(as.numeric(PhraseMatrix[,4]))]

#################################
#Step 4: Tweet'em out!!!
#################################


#Back to the threading based off the initial tweet.
for(j in 1:length(FinalTweetThread)){
  g <- updateStatus(FinalTweetThread[j])
  Sys.sleep(60)
}



