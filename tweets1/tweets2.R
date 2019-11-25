# importing librarys 
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)
library(knitr)
library(stringr)
library(tm)
library(RWeka)
library(wordcloud)
library(formattable)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(reshape2)
library(tidyr)
library(RColorBrewer)
library(networkD3)
library(cluster)
library(viridis)
library(magrittr)
install.packages("magrittr")
install.packages("dplyr")
knitr::opts_chunk$set(message=FALSE, warning=FALSE, echo = FALSE)
str(isis)


# Read data file
isis <- read.csv("/Users/natha/Documents/tweets1/tweets.csv")
tweet <- as.character(isis$tweets)

#removing links
tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)

# removing non-english characters
tweet1 <- grep('tweet',iconv(tweet,'latin1','ASCII',sub='tweet'))
tweet <- tweet[-tweet1]

glimpse(isis)
data=data%>%
        mutate(time=mdy_hm(time))%>%
        mutate(hour=hour(time))%>%
        mutate(wday=wday(time, label = TRUE))%>%
        mutate(month=month(time))%>%
        mutate(year=year(time))%>%
        arrange(time)%>%
        mutate(yr_month=paste(year(time),month(time),sep='-'))%>%
        mutate(date=ymd(paste(year(time),month(time),day(time),sep='-')))%>%
        mutate(is_retweet = ifelse(grepl("^\\bRT\\b", tweets), "Retweets", "Originaltweets"))

#shows the differences between tweets and retweets 
isis %>% mutate(isRT = grepl("^\\RT\\b", tweets)) -> isis
isis %>% summarize("Tweets" = nrow(subset(isis, isRT == FALSE)), 
                   "Retweets" = nrow(subset(isis, isRT == TRUE))) %>%
        formattable(align = "c")        

#Find the influencersTo understand the fanbase, let's look at their number of tweets vs number of follower to get a sense of their activity level and influence.Based on k-mean clustering withinness, we segment their user attributes into 4 groups
fanbase=data%>%
select(name,followers,tweets)%>%
group_by(name)%>%
summarize(n_follower=max(followers),n_tweet=n())

wss <- numeric(15)
for (k in 1:15) {wss[k] <- sum(kmeans(as.matrix(fanbase[, 2:3]), centers=k,nstart=25)$withinss)}


set.seed(20)
ISIScluster <- kmeans(as.matrix(fanbase[, 2:3]), 4, nstart = 20)




##hashtags Let's next look into hashtag applied everyday  

##```{r hashtag}
hash=tweet%>%
        mutate(hash=str_extract_all(tweets, "#\\w+"))%>%
        select(hash)%>%
        filter(!is.na(hash))%>%
        unnest(hash)%>%
        group_by(hash)%>%
        summarize(n_hash=n())%>%
        arrange(desc(n_hash))
hash%>%
        head(10)










