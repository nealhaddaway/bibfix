url <- "https://api.labs.crossref.org/data/retractionwatch"
retwatch_db<-read.csv(url)
write.csv(retwatch_db,  file = 'data/retraction_watch.csv')
