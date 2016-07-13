#need to reduce tweets to get a somewhat interpretable network (2800 unique tweeters in total)
#network analysis and plot

library(twitteR)
library(tidytext)
library(ggplot2)
library(dplyr)
library(igraph)
library(stringr)
#####
###

#This is the redux parsing to generate the basic word lists per tweet
#assuming a clean starting point separate from any of the upstream analysis

hashtag<-'#smbe2016'
tw_list <- searchTwitter(hashtag, n = 1e4, since = '2016-06-31') #~5k tweets
tw_df<-twListToDF(tw_list)



hm<-sort(table(tw_df$screenName))
outdf<-data.frame(screen_name=names(hm),num_tweets=hm)[,c(1,3)]
outdf<-outdf[order(outdf[,2],decreasing=T),]

#OK, start of new code to develop RT network
#code (regex especially!!!) used liberally from
# https://sites.google.com/site/miningtwitter/questions/user-tweets/who-retweet

#TODO:
#replace retweet network construction (not plotting)
#with https://github.com/nfahlgren/conference_twitter_stats/blob/master/retweet_network_generic.R
#it's cleaner and doesn't rely on regex horrors I don't understand

rt_net<-grep("(RT|via)((?:\\b\\W*@\\w+)+)", tw_df$text, 
             ignore.case=TRUE,value=TRUE)
rt_neti<-grep("(RT|via)((?:\\b\\W*@\\w+)+)", tw_df$text, 
              ignore.case=TRUE)

#next, create list to store user names
who_retweet <- as.list(1:length(rt_net))
who_post <- as.list(1:length(rt_net))

# for loop
for (i in 1:length(rt_net))
{ 
  # get tweet with retweet entity
  #nrow= ???
  twit <- tw_df[rt_neti[i],]
  # get retweet source 
  poster<-str_extract_all(twit$text,"(RT|via)((?:\\b\\W*@\\w+)+)")  
  #remove ':'
  poster <- gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] <- gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] <- rep(twit$screenName, length(poster)) 
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
#jpeg('evol2016_top50_twitter_network.jpg',width=960,height=960,pointsize=12)
ggraph(rt_graph,'igraph',algorithm='kk')+
  geom_edge_fan(aes(alpha=retweet),edge_alpha=0.1)+
  geom_node_point(aes(size=num_tweets))+
  geom_node_text(aes(label=name,vjust=-1.5))+
  ggforce::theme_no_axes()+
  theme(legend.position=c(.08,.88))
filename<-paste0(substr(hashtag,2,nchar(hashtag)),'_twitter_network.png')
ggsave(filename,width=7,height=7,dpi=100)
