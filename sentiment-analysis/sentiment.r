rm(list=ls(all=TRUE))
library(data.table)
library(ggplot2)
setwd('D:/Data_Science')



airline <- fread('Airline-Sentiment-2-w-AA.csv')
dim(airline)
##[1] 14640    20

str(airline)   ##display the internal structure of an R object

prop.table(table(airline$airline_sentiment))

# negative   neutral  positive 
# 0.6269126 0.2116803 0.1614071 
# we can infer that most of the sentiments are negative

sentiments <- as.data.frame(prop.table(table(airline$airline_sentiment)))   ##data.frame converts each of its arguments to a data frame 
                                                                            ##using as.data.frame
colnames(sentiments) <- c('Sentiment','Frequency')  ##change col names for new data frame sentiments
sentiments

ggplot(data=sentiments, aes(x=Sentiment,y=Frequency ,fill=Sentiment))+geom_bar(stat = "identity")+ 
  ggtitle("Distribution of sentiments")#we can distinctly observer high negative sentiment


##Finding proportion of tweets for each airline

tweets_per_airlines <- as.data.frame(prop.table(table(airline$airline)))
colnames(tweets_per_airlines)<- c("Airline","Proportion")

ggplot((data=tweets_per_airlines), aes(x=Airline,y=Proportion, fill=Airline))+
geom_bar(stat = "identity")+scale_fill_brewer()+guides(fill=FALSE)+ggtitle("Distribution of tweets per Airline")

##Sentiments for every airline
sentiments_airline <- as.data.frame(prop.table(table(airline$airline_sentiment, airline$airline)))
colnames(sentiments_airline)<- c("Sentiment","Airline","Percent_of_tweets")

cox<- ggplot(data = sentiments_airline,aes(x=Airline,y=Percent_of_tweets, fill=Sentiment))+
      geom_bar(stat = 'identity')
cox+coord_polar() ##this coxcomb diagram shows us 

#splitting the tweet created column for date

newdate <- strptime(as.character(airline$tweet_created), "%m/%d/%Y")
airline$date <- format(newdate, "%y-%m-%d")



ggplot(airline, aes(x=date, y=airline_sentiment, colour=airline)) + geom_line() +
  ggtitle("Airline Sentiment") + xlab("Date") + ylab("Sentiment") 


## negative sentiments across the airlines

neg_reasons <- as.data.frame(prop.table(table(airline$negativereason)))
colnames(neg_reasons) <- c('Reasons','Frequency')
neg_reasons <- neg_reasons[-1,]  #reasons for first one is missing, hence we remove it
neg_reasons

ggplot(airline,aes(x=neg_reasons$Reasons,y=neg_reasons$Frequency,colour=airline)) + geom_line()
