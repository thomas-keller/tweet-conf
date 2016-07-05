library(tidytext)
library(ggplot2)
library(tm)
library(wordcloud)
library(streamR)
library(dplyr)
library(smappR)
library(qdap)
library(qdapRegex)
setwd('~/tweet-pol')
df<-parseTweets('merged_evol2016.json')
#from http://www.r-bloggers.com/playing-with-twitter-data/
#because I am no good at constructing regexes
#removes http links
#nope this is still causing an oom runaway somewhere
#OK, these guys (qdap) actually do text parsing for reals 
#welp, that still breaks the unnesting, so frick it
#df$text<-rm_url(df$text)

tidy_tw<-df %>% unnest_tokens (word,text)
#some of these stop words are unneccessary now that I got a regex working that doesn't core dump
tw_stop<-data.frame(word=c('amp','gt','t.c', 'evol2016','rt','https','t.co','___','1','2','3','4','5','6','7','8','9',"i\'m",'15','30','45','00','10'),lexicon='whatevs')
data("stop_words")

#removes uninformatives words / ones that oversaturate wordcloud (conference hash)
tidy_tw <- tidy_tw %>%
  anti_join(tw_stop)
tidy_tw <- tidy_tw %>%
  anti_join(stop_words)

print(tidy_tw %>% count(word, sort = TRUE)) 

png('evol2016_worldcloud_alldays.png')
fig<-tidy_tw %>%
  count(word) %>%
  with(wordcloud(word, n,remove=c("evol2016"),max.words = 100,colors=brewer.pal(8,'Dark2')))
dev.off()

#prints a list of the most used words, what is visualized the wordcloud
gah<-tidy_tw %>% count(word)
gah2<-as.data.frame(gah[order(gah$n,decreasing=T),])
print(head(gah2,30))

print(nrow(df))
hm<-sort(table(df$screen_name),reversed=T)
outdf<-data.frame(screen_name=names(hm),num_tweets=hm)[,c(1,3)]
outdf<-outdf[order(outdf[,2],decreasing=T),]
write.csv(outdf,file='evol2016_tweetrank.csv',quote=F)
print(head(outdf))

tidy_tw$created_at<-formatTwDate(tidy_tw$created_at)
df$created_at<-formatTwDate(df$created_at)


library(tidyr)
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

evol2016sentiment <- tidy_tw %>%
  inner_join(bing) %>%
  count(id_str, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  inner_join(df[,c(6,10)])

library(cowplot)
library(scales)
library(lubridate)
df_labels<-data.frame(times=strptime(c("2016-06-15 18:00:00","2016-06-17 12:00:00","2016-06-18 7:30:00","2016-06-19 18:30:00","2016-06-21 0:00:00","2016-06-21 18:00:00","2016-06-23 0:00:00"),"%Y-%m-%d %H:%M:%S"),
                      labels=c("anticipation","pre-conf\nworkshops",'conference\ntalks begin','film festival\nsocializing','oh god\nmore talks',"super social!",'bitter\nreality\nintrudes'),
                      y=c(-.1,.7,-.1,.85,0,.55,1.05))
p<-ggplot(evol2016sentiment, aes(created_at, sentiment)) +
  geom_smooth() + xlab("tweet time") + ylab("tweet sentiment")+
  scale_x_datetime(breaks = date_breaks("day")) + background_grid(major = "xy", minor = "none") +
  theme(axis.text.x=element_text(angle=315,vjust=.6)) +
  coord_cartesian(ylim=c(-.5,1.2))+geom_text(data=df_labels,aes(x=times,y=y,label=labels),size=4)
print(p)

save_plot("evol2016_sentiment_time.png",p)

p2<-qplot(outdf$num_tweets.Freq)+scale_x_log10()+xlab("number #evol2016 tweets")+ylab("number of tweeters")
print(p2)
save_plot("evol2016_numtweets_user.png",p2)

#need to reduce tweets to get a somewhat interpretable network (2800 unique tweeters in total)
#network analysis and plot

library(tidytext)
library(ggplot2)
library(tm)
library(wordcloud)
library(streamR)
library(dplyr)
library(smappR)
library(qdap)
library(qdapRegex)
library(igraph)
library(stringr)
#####
###

#This is the redux parsing to generate the basic word lists per tweet
#assuming a clean starting point separate from any of the upstream analysis
df<-parseTweets('merged_evol2016.json')
#from http://www.r-bloggers.com/playing-with-twitter-data/
#because I am no good at constructing regexes
#removes http links
#nope this is still causing an oom runaway somewhere
#OK, these guys (qdap) actually do text parsing for reals 
#welp, that still breaks the unnesting, so frick it
#df$text<-rm_url(df$text)

tidy_tw<-df %>% unnest_tokens (word,text)
#some of these stop words are unneccessary now that I got a regex working that doesn't core dump
tw_stop<-data.frame(word=c('amp','gt','t.c', 'evol2016','rt','https','t.co','___','1','2','3','4','5','6','7','8','9',"i\'m",'15','30','45','00','10'),lexicon='whatevs')
data("stop_words")

#removes uninformatives words / ones that oversaturate wordcloud (conference hash)
tidy_tw <- tidy_tw %>%
  anti_join(tw_stop)
tidy_tw <- tidy_tw %>%
  anti_join(stop_words)


outdf<-data.frame(screen_name=names(hm),num_tweets=hm)[,c(1,3)]
outdf<-outdf[order(outdf[,2],decreasing=T),]

#OK, start of new code to develop RT network
#code (regex especially!!!) used liberally from
# https://sites.google.com/site/miningtwitter/questions/user-tweets/who-retweet
rt_net<-grep("(RT|via)((?:\\b\\W*@\\w+)+)", df$text, 
             ignore.case=TRUE,value=TRUE)
rt_neti<-grep("(RT|via)((?:\\b\\W*@\\w+)+)", df$text, 
             ignore.case=TRUE)

#next, create list to store user names
who_retweet <- as.list(1:length(rt_net))
who_post <- as.liset(1:length(rt_net))

# for loop
for (i in 1:length(rt_net))
{ 
  # get tweet with retweet entity
  #nrow= ???
  twit <- df[rt_neti[i],]
  # get retweet source 
  poster<-str_extract_all(twit$text,"(RT|via)((?:\\b\\W*@\\w+)+)")  
  #remove ':'
  poster <- gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] <- gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] <- rep(twit$screen_name, length(poster)) 
}

# unlist
who_post <- unlist(who_post)
who_retweet <- unlist(who_retweet)

####
#Preprocessing the dataframes as as contacts to something
#igraph likes

#I guess I need an edge aesthetic for ggraph to paint with
retweeter_poster <- data.frame(from=who_retweet, to=who_post,retweets=1)

#filters out some bad parsing and users who arent in the node graph
#node_df has the screen_name and number of tweets per user, which will serve as the vertex/node dataframe
#in igraph speak
node_df<-outdf
names(node_df)<-c("id","num_tweets")
#This step #REALLLY IMPORTANT# for plotting purposes, determines how dense the network is
#need to tune based on how big you want your input network is
node_df2<-droplevels(node_df[1:50,]) #selecting only the top 50 posting from #evol2016 for plotting purposes
filt_rt_post<-retweeter_poster[retweeter_poster$from %in% node_df2$id & retweeter_poster$to %in% node_df2$id,]
filt_rt_post<-droplevels(filt_rt_post) #ditch all those fleshbags that had to talk to people instead of tweeting
head(filt_rt_post)

#this creates a directed graph with vertex/node info on num_tweets, and edge info on retweets
rt_graph<-graph_from_data_frame(d=droplevels(filt_rt_post),vertices=droplevels(node_df2),directed=T)

#simplify the graph to remove any possible self retweets since now twitter is dumb any allows that
#and any multiple edges
#have to wait a couple seconds to let graph be generated before simplify call
#merge all the multiple rts a person has into one edge to simplify visualization
rt_graph<-simplify(rt_graph,remove.multiple=T,remove.loops=TRUE,edge.attr.comb='sum')

###
#Plotting using ggraph

library(ggraph)
library(ggplot2)
jpeg('evol2016_top50_twitter_network.jpg',width=960,height=960,pointsize=12)
g1<-ggraph(rt_graph,'igraph',algorithm='kk')+
  geom_edge_fan(aes(alpha=retweet),edge_alpha=0.1)+
  geom_node_point(aes(size=num_tweets))+
  geom_node_text(aes(label=name,vjust=-1.5))+
  ggforce::theme_no_axes()+
  theme(legend.position=c(.08,.88))
dev.off()




  
  
  
  
  
  
  
# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

#### 
# Making the contact network
#two column matrix of edges
retweeter_poster = cbind(who_retweet, who_post)

# generate graph
rt_graph = graph.edgelist(retweeter_poster)

# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

###
#First attempt at making a figure with igraph
# choose some layout
glay = layout.fruchterman.reingold(rt_graph)

# plot
par(bg="gray15", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=10,
     vertex.label=ver_labs,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=3,
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
# add title
title("\nTweets with 'bioinformatics':  Who retweets whom",
      cex.main=1, col.main="gray95") 

#A more elaborate way could be to map out @mentions, but that's for another time
