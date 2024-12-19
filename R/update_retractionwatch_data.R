#URL is from https://www.crossref.org/documentation/retrieve-metadata/retraction-watch/. Previously, we used crossref's own labs API link (https://www.crossref.org/labs/retraction-watch/) but Crossref now recommend sourcing off gitlab

#object for url
url <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv"
#read csv from url
retwatch_db<-read.csv(url)
#update the csv in the 'data' directory
write.csv(retwatch_db,  file = 'data/retraction_watch.csv')

