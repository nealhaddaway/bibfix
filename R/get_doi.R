#' Get digital object identifiers
#' 
#' @description Retrieve digital object identifiers (DOIs) for records 
#' that are missing them. The function 
#' @param bib_data A bibliographic data set stored as a dataframe, with 
#' each record as a row. Records must contains a column called 'title' 
#' that contains the record title, and a column called 'doi' that 
#' contains (or would contain) the DOIs.
#' @return A revised dataframe containing DOIs for records for which 
#' the DOI could be retrieved.
#' @importFrom rcrossref cr_works
#' @importFrom stringdist stringdist
#' @export
#' @examples 
#' \dontrun{
#' bib_data<-revtools::read_bibliography('inst/extdata/export.ris')
#' }
get_doi <- function(bib_data){
  
  #subset records that are missing a DOI
  missing_doi <- subset(bib_data, is.na(doi) == TRUE)
  
  
  
  
  
  report <- paste0('Your bibliographic dataset contained ',
                   nrow(missing_doi),
                   ' records without digital object identifiers.')

  
  # GET DOIS FROM CROSSREF -----------------------------------------------
  alldists<-c()
  
  for (a in 1:nrow(missing_doi)){
    #send the title of the reference as a query to CrossRef and get reuslts back
    results<-tryCatch(
      rcrossref::cr_works(query=missing_doi$title[a], sort = 'relevance'),
      error=function(e) NULL)
    
    #check if no results, if so, return NA then iterate the loop
    if(is.null(results)==T){
      print(paste(a,') ', NA, sep=''))
      missing_doi$doi[a]<-NA
      alldists<-c(alldists,NA)
      a<-a+1
      
    } 
    
    #if there are results, return them, find nearest match to title and get the doi
    else
    {
      #extract the bibliographic info
      results_data<-results$data
      #generate vector of distances between your bibliography title and that of crossref
      dists<-stringdist::stringdist(missing_doi$title[a],
                                    results_data$title)
      #For those where the distance is over 20 characters, coerce to NA (based on brief tests, this is the threshold of similarity)
      dists[dists>20]<-NA
      #find the index of the nearest matching title in the CrossRef table
      nearestmatch<-which(dists==min(dists, na.rm = T))
      #snatch its DOI and add to your bibliographic dataframe
      missing_doi$doi[a]<-results_data[nearestmatch,]$doi[1]
      #optional print of doi result, good for bug testing/development
      #print(paste(a,') ', results_data[nearestmatch,]$doi[1], sep=''))
    }       
    
  }
  
  
  return()
  
}
