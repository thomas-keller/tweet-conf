load('my_oauth')
library(streamR)

#collecting all tweets that contain at least one of the following terms
#following Barbera et al. 2015  tweeting from left to right

# this script will be run once every hour, and tweets are stored in different
# files, whose name indicate when they were created.
current.time <- format(Sys.time(), "%Y-%m-%d-%H-%M")
f <- paste0("evol2016_", current.time, '.json')


keywords<-c('evol2016')
filterStream(file.name=f, track = keywords, timeout = 3600, oauth = my_oauth)

