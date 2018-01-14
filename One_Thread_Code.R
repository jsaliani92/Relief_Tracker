#New Script:
##This script will bring all disasters into one thread to be released at one point in the day
##The advantage of this style is to bring the disasters into one thread
##While also highlighting individual efforts for those disasters
##Get in the impact of how many days it's been for each disaster
##And highlight different organizations, not just the big ones like @CharityNav and @GlobalGiving

#Structure:
##First tweet: Todays date plus "It's been" message
##Second tweet: # of days since disaster plus custom charity message afterwards.
##Each tweet shall be pre-written with the exception of the day's since section

#How do I feel about this?:
##Awesome-sauce

#Notes
##I also need to add a blog post to this to summarize the point of this creation
##Since I haven't done that yet

#Begin

###############
#Set up
###############


setwd("/Users/juliansaliani/Desktop/Misc.(Random)/Tracker_Code/")

Overall <- read.csv('Overall_Chart.csv')
Donation <- read.csv('Donation_Type.csv')
Started <- read.csv('Started_Type.csv')
api_keys <- read.csv("Hurricane_Twitter_Account.csv",header = FALSE,stringsAsFactors = FALSE)


#Load Libraries

library(rvest)
library(stringr)
library(dplyr)
library(twitteR)


###################
#Edited the tweet function to do over 140 charasters.
#changed to 280!!


#if (nchar(text) > 140 && !bypassCharLimit) 
#  stop("Status can not be more than 140 characters")


# Set up twitter api

setup_twitter_oauth(consumer_key = api_keys[1,2],
                    consumer_secret = api_keys[2,2],
                    access_token = api_keys[3,2],
                    access_secret = api_keys[4,2])



time <- Sys.time()


#######################################
#Phase 0: Update Update Status Function
#######################################



#############################
#Phase 1: Initial Tweet
#############################


tweet1 <- paste0("Today is ",format(Sys.Date(),"%b")," ",format(Sys.Date(),"%d"),", ",format(Sys.Date(),"%Y"),". These recovery efforts have fallen from the news cycle. We should not and cannot forget about them. Whether you can volunteer, donate, or just simply spread the word, lets help sustain these efforts today. #ReliefEfforts")
tweet1extra <- paste0("Today is ",format(Sys.Date(),"%b")," ",format(Sys.Date(),"%d"),", ",format(Sys.Date(),"%Y"),". These recovery efforts")


################################
#Phase 2: Days been Ind Tweet and create loop for all disasters.
################################

tweet_texts <- vector("list",length=6)


#Put together the Disaster along with the appropriate starting language
#Meant to be more unique than just: this disaster happened

PhraseMatrix <- matrix(ncol=3,nrow=dim(Overall)[1])
for(i in 1:(dim(Overall)[1])){
Phrase <- vector("list",length=4)
day <- as.numeric(as.Date(Sys.time()) - as.Date(Overall[i,4],"%m/%d/%y"))
PhraseMatrix[i,1] <- paste0(day," days since")
PhraseMatrix[i,2] <- paste0(Overall[i,1]," ",Started[Overall[i,5],2],".")
PhraseMatrix[i,3] <- i
}

#Identify the number of days since the disaster hit



#####################################
#Phase 3: Setting Up Database Current
#####################################


#Pull in sheets
###############

setwd("~/Desktop/Misc.(Random)/Tracker_Code")
Uncommon <- read.csv('UncommonCharities.csv')
Common <- read.csv('CommonCharities.csv')
CommonList <- read.csv('CommonList.csv')

#Summarize Tables
library(dplyr)

#Common charities
total <- length(unique(Uncommon[,1]))
events <- unique(Common[,2])
events <- events[order(unlist(count(Common,Charity)[,2]))]
Exportlist <- vector("list",length=total)
ChoosingCommon <- Common

#Formulate Tweet Matrix
TweetMatrix <- as.data.frame(matrix(ncol=3,nrow=total))
  
for(i in events){
  choosing <- which(ChoosingCommon[,2] == i) #vector that randomly choses an disaster for this charity
  choosinglen <- length(choosing)
  chosen <- round(runif(1,1,choosinglen),0)
  Exportlist[[grep(i,events)]] <- ChoosingCommon[choosing[chosen],]
  Disasterdelete <- ChoosingCommon[choosing[chosen],1]
  Charitydelete <- ChoosingCommon[choosing[chosen],2]
  elimination <- unique(c(which(Disasterdelete==ChoosingCommon[,1]),which(Charitydelete==ChoosingCommon[,2])))
  ChoosingCommon <- ChoosingCommon[-elimination,]
  TweetMatrix[grep(i,events),1] <- Exportlist[[grep(i,events)]][1]
  TweetMatrix[grep(i,events),2] <- as.vector(as.matrix(CommonList[as.numeric(Exportlist[[grep(i,events)]][2]),1]))
  TweetMatrix[grep(i,events),3] <- as.vector(as.matrix(Exportlist[[grep(i,events)]][3]))
}



#Formulate Tweets

#No do remaining disasters with Uncommon Charities
##################################################

unExportlist <- unlist(Exportlist)
ChosenDisasters <- unExportlist[grep("Disaster",names(unExportlist))]
UnCommonFinal <- Uncommon[-which(is.na(match(Uncommon[,1],ChosenDisasters)) == FALSE),]
Disastersleft <- unique(UnCommonFinal[,1])
events <- unique(UnCommonFinal[,1])

for(i in Disastersleft){
  UnCommonTemp <- UnCommonFinal[which(UnCommonFinal[,1] == i),]
  choosinglen <- dim(UnCommonTemp)[1]
  chosen <- round(runif(1,1,choosinglen),0)
  Exportlist[[grep(i,events)+length(unique(Common[,2]))]] <- UnCommonTemp[chosen,]
  TweetMatrix[grep(i,events)+length(unique(Common[,2])),1] <- Exportlist[[grep(i,events)+length(unique(Common[,2]))]][1]
  TweetMatrix[grep(i,events)+length(unique(Common[,2])),2] <- as.vector(as.matrix(Exportlist[[grep(i,events)+length(unique(Common[,2]))]][2]))
  TweetMatrix[grep(i,events)+length(unique(Common[,2])),3] <- as.vector(as.matrix(Exportlist[[grep(i,events)+length(unique(Common[,2]))]][3]))
}

#Pull in Charity Names

TweetMatrix

#### Complete :) ####


#####################################
#Phase 4: Put together charity tweets
#####################################


#Put in correct order
TweetMatrix <- TweetMatrix[order(as.numeric(as.character(TweetMatrix[,1]))),]

OverallMat <- cbind(PhraseMatrix,TweetMatrix,Overall[,6])

OrderThread <- vector(length=dim(OverallMat)[1])

TweetThread <- vector(length=dim(OverallMat)[1])

for(i in 1:length(TweetThread)){
  OrderThread[i] <- strsplit(as.vector(OverallMat[i,1])," ")[[1]][1]
}


OverallMat <- OverallMat[order(as.numeric(as.character(OrderThread))),]

for(i in 1:length(TweetThread)){
  TweetThread[i] <- as.character(paste0("It's been ",OverallMat[i,1]," ",OverallMat[i,2],"\n@",OverallMat[i,5]," ",OverallMat[i,6],"\n",OverallMat[i,7]))
}

############################
#Phase 5: Tweet first result
############################

# send tweet

#f <- tweet(tweet1)

Sys.sleep(180)

#Back to the threading based off the initial tweet.
for(i in 1:length(TweetThread)){
  g <- tweet(TweetThread[[i]])
  Sys.sleep(180)
}


#####################
#Excess Scripts
#####################


#####################
#Test tweet within a thread
#for(i in 1:length(TweetThread)){
#updateStatus(TweetThread[[i]],inReplyTo = idtest)
#Sys.sleep(60)
#}

###### If you want to link by tweet.
#for(i in 3:length(TweetThread)){
#  
#  tweetdb <- unlist(searchTwitter(paste("#ReliefEfforts"), n=25))
#  tweetdb2 <- twListToDF(c(tweetdb,f)) # converting to df
#  tweetdb3 <- tweetdb2[grep(paste0(PhraseMatrix[(i-1),1]," ",PhraseMatrix[(i-1),2]),tweetdb2[,1])[1],]
#  idtest <- tweetdb3[1,8]
#  updateStatus(TweetThread[[i]],inReplyTo = idtest)
#Sys.sleep(15)
#
#}



