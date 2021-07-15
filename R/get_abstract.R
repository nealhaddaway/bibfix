#' Get abstracts for bibliographic records missing them
#' 
#' @description Retrieve abstracts for records that are missing them. 
#' The function uses the crossref API matching on DOI (digital object 
#' identifier).
#' @param bib_data A bibliographic data set stored as a dataframe, with 
#' each record as a row. Records must contains a column called 'abstract' 
#' that contains the record title, and a column called 'doi' that 
#' contains (or would contain) the DOIs.
#' @return A revised dataframe containing records for which the abstract 
#' could be retrieved.
#' @importFrom rcrossref cr_abstract
#' @export
#' @examples 
#' \dontrun{
#' bib_data <- synthesisr::read_refs('inst/extdata/export.ris')
#' bib_data_new <- get_abstract(bib_data)
#' }
get_abstract <- function(bib_data){
  
  #subset records that are missing a DOI
  missing_abstract <- subset(bib_data, is.na(abstract) == TRUE)
  
  #first try records with a DOI
  doi_query <- subset(missing_abstract, is.na(doi) == FALSE)
  
  # Get abstracts from crossref -----------------------------------------------
  results <- data.frame(doi = NULL, abstract = NULL)
  
  #start timer
  t0 <- Sys.time()
  pb <- txtProgressBar(min = 0, max = nrow(doi_query), style = 3)
  for (a in 1:nrow(doi_query)){
    #send the doi of the record as a query to CrossRef and get results back
    data <- data.frame(doi = doi_query$doi[a],
                       abstract = 
                         tryCatch(
                           rcrossref::cr_abstract(doi = doi_query$doi[a]), 
                           error=function(e) {return(NA)}))
    setTxtProgressBar(pb, a)
    #print(paste0('record ', a, ' retrieved'))
    results <- rbind(results, data)
  }
  #stop timer
  t1 <- Sys.time()
  #elapsed time
  time <- t1 - t0
  
  #join and tidy up the results
  returns <- nrow(subset(results, is.na(abstract) == FALSE))
  results <- merge(x = bib_data, y = results, by = "doi", all.x = TRUE)
  results <- subset(results, select = -c(abstract.x))
  results <- dplyr::rename(results, abstract = abstract.y)
  
  #compile report and print to console
  report <- paste0('Your bibliographic dataset contained ',
                   nrow(missing_abstract),
                   ' records without abstracts. ',
                   nrow(doi_query),
                   ' of these records also had DOIs and ',
                   returns,
                   ' of these records had abstracts on CrossRef. Your request took ',
                   round(time, digits = 2),
                   ' minutes to complete.')
  print(report)
  
  return(results)
  
}
