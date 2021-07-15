#' Get URL links
#' 
#' @description Retrieve URL links to the record in various formats and 
#' from various sources.
#' @param bib_data A bibliographic data set stored as a dataframe, with 
#' each record as a row. Records must contains a column called 'doi' that 
#' contains the DOIs for the records.
#' @return A revised dataframe containing various urls for records that 
#' could be found using the API.
#' @importFrom roadoi oadoi_fetch
#' @export
#' @examples 
#' \dontrun{
#' bib_data<-revtools::read_bibliography('inst/extdata/export.ris')
#' }
get_urls <- function(bib_data){
  
  #if bibliographic data lacks url field, add an empty one
  if (is.element('url', colnames(bib_data)) == FALSE){
    bib_data$url <- NA
  }
  
  #subset records that are missing a url
  missing_url <- subset(bib_data, is.na(url) == TRUE)
  
  #subset that have a doi
  url_query <- subset(missing_url, is.na(doi) == FALSE)
  
  #reset results
  results <- data.frame(doi = NULL, abstract = NULL)
  
  # Source 1: Unpaywall via roadoi ----
  #start timer
  t0 <- Sys.time()
  for (a in 1:nrow(url_query)){
    #send the doi of the record as a query to CrossRef and get results back
    response <- tryCatch(
      suppressWarnings(roadoi::oadoi_fetch(dois = url_query$doi[a],
                          email = 'neal.haddaway@sei.org')),
      error=function(e) {return(NA)})
    
    url_best <- do.call(rbind.data.frame, response[[2]])$url
    if(is.null(url_best) == TRUE){
      url_best <- NA
    }
    url_pdf <- do.call(rbind.data.frame, response[[2]])$url_for_pdf
    if(is.null(url_pdf) == TRUE){
      url_pdf <- NA
    }
    oa_status <- response$oa_status
    
    #build dataframe
    data <- data.frame(doi = url_query$doi[a],
                       url = url_best,
                       pdf = url_pdf,
                       oa_status = oa_status)
    results <- rbind(results, data)
  }
  #stop timer
  t1 <- Sys.time()
  #elapsed time
  time <- t1 - t0
  
  #join and tidy up the results
  returns <- nrow(subset(results, is.na(url) == FALSE))
  results <- merge(x = bib_data, y = results, by = "doi", all.x = TRUE)
  results <- subset(results, select = -c(abstract.x))
  results <- dplyr::rename(results, abstract = abstract.y)
  
  
  
  report <- paste0('Your bibliographic dataset contained ',
                   nrow(missing_doi),
                   ' records without digital object identifiers.')

  print(report)
  return()
  
}
