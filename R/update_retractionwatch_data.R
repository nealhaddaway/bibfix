#url is from https://www.crossref.org/labs/retraction-watch/

#object for url
url <- "https://raw.githubusercontent.com/nealhaddaway/bibfix/refs/heads/master/data/retraction_watch.csv"
#read csv from url
retwatch_db<-read.csv(url)
#update the csv in the 'data' directory
write.csv(retwatch_db,  file = 'data/retraction_watch.csv')
