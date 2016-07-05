library(twitteR)
#see instructions about setting up a twitter authorization, this will give you the 4 tokens you need
#I've stored them in my local Rdata
load('twitter-secrets.Rdata')
setup_twitter_oauth(cons_key,cons_sec, acc_tok, acc_sec)

tw <- searchTwitter('#isemph', n = 1e4, since = '2016-06-22')
tw3 <- searchTwitter('#icrs2016', n = 2e4, since = '2016-06-18') #11200

t4 <- searchTwitter('#isemph', n = 1e4, since = '2016-06-22')
t5 <- searchTwitter('#icrs2016', n = 2e4, since = '2016-06-18') #11200

coral_tw<-twListToDF(tw3)
write.write(coral_tw,file='isemph_tweets.csv',row.names=False)