

## Loading the libraries

library(ggplot2)
install.packages("ggplot2")
library(lubridate)
install.packages("lubridate")

#?dplyr
library(scales)
install.packages("scales")

library(tm)
install.packages("tm")

library(stringr)
install.packages("stringr")

library(wordcloud)
install.packages("wordcloud")

library(RColorBrewer)
install.packages("RColorBrewer")

library(syuzhet)
install.packages("syuzhet")

library(reshape2)
install.packages("reshape2")

library(dplyr)
install.packages("dplyr")

library(twitteR)
install.packages("twitteR")



## Importing the tweets from dataset
twitter <- read.csv("C:/Users/DELL/Desktop/Twitter_Test_SVM.csv", header = T, sep = ";") 
class(twitter)
View(twitter)

## Parsing the date and time of the tweets
twitter$`Created.At` <- dmy_hm(twitter$`Created.At`)
## Allocating to one timezone
twitter$`Created.At` <- with_tz(twitter$`Created.At`, "UTC")

## Plotting the tweets by month
ggplot(data = twitter, aes(x = month(`Created.At`, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Month") + ylab("Number of tweets") +
  scale_fill_gradient(low = "blue", high = "red")

## Grouping tweets by time
twitter$timeonly <- as.numeric(twitter$`Created.At` - trunc(twitter$`Created.At`, "days"))
class(twitter$timeonly) <- "POSIXct"
install.packages('readr')
library(readr)

## Plotting by time
ggplot(data = twitter, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..),binwidth = 30) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_x_datetime(breaks = date_breaks("3 hours"), 
                   labels = date_format("%H:00")) +
  scale_fill_gradient(low = "blue", high = "red")


## Cleaning text
twitter$clean_text <- str_replace_all(twitter$Text, "@\\w+", "")

## Sentiment analysis
Sentiment <- get_nrc_sentiment(twitter$clean_text)
tweets_senti <- cbind(twitter, Sentiment)
View(tweets_senti)
write.csv(tweets_senti,'C:/Users/DELL/Desktop/Tweets_Obama.csv', row.names = FALSE)


## Total of the sentiment weights
sentimentTotals <- data.frame(colSums(tweets_senti[,c(15:22)]))
names(sentimentTotals) <- "count"

## Joining the datasets
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL


## Plotting the sentiment scores
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")


## Positive and negative tweets
posnegtime <- tweets_senti %>% 
  group_by(created = cut(`Created.At`, breaks="10 hour")) %>%
  summarise(negative = mean(negative),
            positive = mean(positive)) %>% melt

## Naming the dataframe
names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])


## Plotting the tweets
ggplot(data = posnegtime, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
  geom_line(size = 1.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.3) +
  ylim(0, NA) + 
  scale_colour_manual(values = c("green", "red")) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  scale_x_time(breaks = waiver(), minor_breaks = waiver(), 
               labels = date_format("%h-%m")) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment Over Time")


## Grouping by emotion

tweets_senti$day <- wday(tweets_senti$`Created.At`, label = TRUE)
dailysentiment <- tweets_senti %>% group_by(day) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% melt

## Variation in sentiment by day
names(dailysentiment) <- c("day", "sentiment", "meanvalue")


## Plotting the emotion variation by day
ggplot(data = dailysentiment, aes(x = day, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment During the Year")



