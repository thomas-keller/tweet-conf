# tweet-conf
Some simple R scripts to collect and visualize twitter happenings around a conference hashtag. Currently the "organization," such as it is, mainly lies in the one larger script gettweets_ex.R that has all the code to make the figures that I talk about in this [blog post](http://thomas-keller.github.io/articles/I-analyzed-evolution-2016-twitter-and-you-can-too-for-other-conferences/). There are also a couple short scripts that demonstrates how to pull down tweets with twitteR or streamR. TwitteR should be much more practicle for most conference scale events, unless they are approaching 10's of thousands, then you might need to go to streamR.

The script covers making a wordcloud, sentiment analysis of a conference through time, and making a network graph of the top twitter users. There's lots of room for improvement in these analyses, so let me know if there's something I should be doing better! The two main scripts for now are "tweet_sent_cloud.R" and "retweet_network_ggraph.R" .

To get started you need to first register an "app" with [Twitter](https://apps.twitter.com/). Go to that website and after logging in with your account and following the instructions you can click over to the Key and Token tab to get the tokens you need to copy in to the follow one time section (it will be saved into a file that will be used for future uses).

```R
library(twitteR)
library(ROAuth)
cons_key=xxx
cons_sec=xxx
acc_tok=xxx 
acc_sec=xxx 
#fill in these X's with your tokens from https://apps.twitter.com/ 
#DO NOT WRITE THESE FOUR VALUES INTO CODE YOU POST ONTO GITHUB ETC. THEY ARE SECRET FOR A REASON!
#keep in a local file
#this is what I do to reduce the hassle
load('twitter-secrets.Rdata')
setup_twitter_oauth(cons_key,cons_sec, acc_tok, acc_sec)
#it should ask you if you want to set up a local file to save the details for future use
#I suggest yes
#that's it!
```



