library(twitteR)
library(ROAuth)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(dplyr)


#from SmappR https://github.com/SMAPPNYU/smappR/
#By Pablo Barbera http://pablobarbera.com/ and others

formatTwDate <- function(datestring, format="datetime"){
  if (format=="datetime"){
    date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
  }
  if (format=="date"){
    date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
  }   
  return(date)
}


#search along the hashtag (can be have multiple hashtags if you want/need)
#convert to dataframe
hashtag<-'#ecmtb2016'
hashtag<-'#smbe16'
tw_list <- searchTwitter(hashtag, n = 1e4, since = '2016-07-08') #~5k tweets
#tw_df<-read.csv('smbe16.csv',header=T,stringsAsFactors=FALSE) # the stringsasfactors always get me
tw_df<-twListToDF(tw_list)
tw_df<-unique(tw_df)

users<-data.frame(word=tolower(tw_df$screenName),lexicon=rep('whatevs',nrow(tw_df)))
#breaks down tweets into words for tidy (word) level analyses
tidy_tw<-tw_df %>% unnest_tokens(word,text)


#removes uninformatives words / ones that oversaturate wordcloud
tw_stop<-data.frame(word=c(hashtag,'amp','gt','t.c','rt','https','t.co','___','1','2','3','4','5','6','7','8','9',"i\'m",'15','30','45','00','10'),lexicon='whatevs')
data("stop_words")
tidy_cloud <- tidy_tw %>%
 anti_join(tw_stop) %>%
  anti_join(stop_words) %>%
  anti_join(users)

print(tidy_cloud %>% count(word, sort = TRUE)) 

filename<-paste0(substr(hashtag,2,nchar(hashtag)),"_wordcloud.png")
png(filename, width=12, height=8, units="in", res=300)
tidy_cloud %>%
 count(word) %>%
 with(wordcloud(word, n,max.words = 100,colors=brewer.pal(8,'Dark2')))
dev.off()


#prints a list of the most used words, what is visualized the wordcloud
topw<-tidy_cloud %>% count(word)
topw<-as.data.frame(topw[order(topw$n,decreasing=T),])
print(head(topw,30))

#plots a ranking of the most active twitter users of the hashtag over the conference
#this section also comes from https://github.com/nfahlgren/conference_twitter_stats/blob/master/conf_twitter_stats.R
#plot and code ultimately derives from http://www.gettinggeneticsdone.com/2012/07/plotting-frequency-of-twitter-hashtag.html
# Make a table of the number of tweets per user
user.tweets <- as.data.frame(table(tw_df$screenName))
names(user.tweets) <- c("User", "Tweets")

# Order the table by number of tweets per user & do some culling
user.tweets <- user.tweets[with(user.tweets, order(-Tweets)), ]
user.tweets_fig<-user.tweets[user.tweets$Tweets>=2,]
user.tweets_fig<-user.tweets_fig[1:40,]

#make the plot for the top 40 or so
#I normally hate the x and y guide lines, but they serve a purpose with the extreme skew and names
ggplot(data=user.tweets_fig, aes(x=reorder(User, Tweets), y=Tweets)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous("Tweets") +
  scale_x_discrete("User") +
  labs(title = paste(hashtag, " tweets per user")) +
  theme_bw() +
  theme(axis.title = element_text(face="bold"), axis.text.y = element_text(size=6))
filename<-paste0(substr(hashtag,2,nchar(hashtag)),"_tweetrank.png")
ggsave(filename,width=7,height=7,dpi=100)




tidy_tw$created<-formatTwDate(tidy_tw$created)
tw_df$created<-formatTwDate(tw_df$created)


library(tidyr)
bing <- sentiments %>%
 filter(lexicon == "bing") %>%
 select(-score)

conf_sent <- tidy_tw %>%
 inner_join(bing) %>%
 count(id, sentiment) %>% 
 spread(sentiment, n, fill = 0) %>%
 mutate(sentiment = positive - negative) %>%
 inner_join(tw_df[,c(5,8)]) #join on id and created

library(cowplot)
library(scales)
library(lubridate)

#adjust time zone of tweets with lubridate
conf_sent$created<-ymd_hms(conf_sent$created,tz='AEST')

#Example could include label, but don't have time to figure out what is driving
#inflection points of moods during these other conferences
#df_labels<-data.frame(times=strptime(c("2016-06-15 18:00:00","2016-06-17 12:00:00","2016-06-18 7:30:00","2016-06-19 18:30:00","2016-06-21 0:00:00","2016-06-21 18:00:00","2016-06-23 0:00:00"),"%Y-%m-%d %H:%M:%S"),
#                      labels=c("anticipation","pre-conf\nworkshops",'conference\ntalks begin','film festival\nsocializing','oh god\nmore talks',"super social!",'bitter\nreality\nintrudes'),
#                      y=c(-.1,.7,-.1,.85,0,.55,1.05))
ggplot(conf_sent, aes(created, sentiment)) +
 geom_smooth() + xlab("tweet time") + ylab("tweet sentiment")+
 scale_x_datetime(breaks = date_breaks("day")) + background_grid(major = "xy", minor = "none") +
 theme(axis.text.x=element_text(angle=315,vjust=.6)) +
 coord_cartesian(ylim=c(-.5,1.2)) #+geom_text(data=df_labels,aes(x=times,y=y,label=labels),size=4)


filename<-paste0(substr(hashtag,2,nchar(hashtag)),"_sent_time.png")
ggsave(file=filename,width=7,height=7,dpi=100)

p2<-qplot(outdf$num_tweets.Freq)+scale_x_log10()+xlab("number #evol2016 tweets")+ylab("number of tweeters")
print(p2)
save_plot("evol2016_numtweets_user.png",p2)