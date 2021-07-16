#' Get abstracts for bibliographic records missing them
#' 
#' @description Retrieve abstracts for records that are missing them. 
#' The function uses the crossref API matching on DOI (digital object 
#' identifier).
#' @param bib_data A bibliographic data set stored as a dataframe, with 
#' each record as a row. Records must contains a column called 'abstract' 
#' that contains the record title, and a column called 'doi' that 
#' contains (or would contain) the DOIs.
#' @param source Source to use for obtaining information. Options are 
#' CrossRef ('crossref') or The Lens.org ('lens'). The former is free 
#' and requires only registration of an email address within the R 
#' environment. The Lens.org requires application for a free, 
#' time-limited API token. The ShinyApp includes token-free access. 
#' Other users must request a Lens.org token here: 
#' https://www.lens.org/lens/user/subscriptions#scholar.
#' @return A revised dataframe containing records for which the abstract 
#' could be retrieved.
#' @importFrom rcrossref cr_abstract
#' @export
#' @examples 
#' \dontrun{
#' bib_data <- synthesisr::read_refs('inst/extdata/export.ris')
#' bib_data_new <- get_abstract(bib_data)
#' bib_data_new <- get_abstract(bib_data, source = 'lens', token = token)
#' }
get_abstract <- function(bib_data,
                         source = 'crossref',
                         token = NULL){
  
  bib_data <- bib_data
  
  #subset records that are missing a DOI
  missing_abstract <- subset(bib_data, is.na(abstract) == TRUE)
  
  #first try records with a DOI
  doi_query <- subset(missing_abstract, is.na(doi) == FALSE)
  
  ## Get abstracts from crossref (not as effective as Lens.org)-----------------------------------------------
  if (identical(source, 'crossref') == TRUE){
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
                     ' of these records had abstracts on CrossRef.')
  }
  
  
  # Get abstracts from Lens.org (preferable since it includes CrossRef) -----------------------------------------------
  if (identical(source, 'lens') == TRUE){
    request <- paste0('{
	"query": {
		"terms": {
			"doi": [', paste0('"', paste(doi_query$doi, collapse = '", "'), '"'), ']
		}
	},
	"size":500,
	"scroll": "1m",
	"include": ["external_ids", "abstract"]
}')
    #"lens_id", "authors", "publication_type", "title", "external_ids", "start_page", "end_page", "volume", "issue", "references", "scholarly_citations", "source_urls", "abstract", "date_published", "year_published", "references_count", "scholarly_citations_count", "source"
    
    # perform search and extract results - this requires an API token
    data <- getLENSData(token, request)
    
    # report requests remaining within the limit (currently 50/min)
    requests_remaining <- data[["headers"]][["x-rate-limit-remaining-request-per-minute"]]
    print(paste0('Remaining requests = ', requests_remaining))
    
    # extract the JSON content of the response
    record_json <- httr::content(data, "text")
    
    # convert json output from article search to list
    record_list <- jsonlite::fromJSON(record_json) 
    results <- as.data.frame(record_list)
    
    #extract dois
    results$doi <- unlist(lapply(results$data.external_ids, function(ch) expss::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    results <- data.frame(abstract = results$data.abstract, doi = results$doi)
    
    #join and tidy up the results
    returns <- nrow(subset(results, is.na(results$abstract) == FALSE))
    results <- merge(x = bib_data, y = results, by.x = 'doi', by.y = 'doi', all.x = TRUE)
    #lens_abstract <- results$abstract[match(bib_data$doi, results$doi)]
    
    #merge the two incomplete abstract fields 
    results <- dplyr::mutate(results, abstract = dplyr::coalesce(abstract.x, abstract.y))
    results <- subset(results, select = -c(abstract.x, abstract.y))
    
    #compile report and print to console
    report <- paste0('Your bibliographic dataset contained ',
                     nrow(missing_abstract),
                     ' records without abstracts. ',
                     nrow(doi_query),
                     ' of these records also had DOIs and ',
                     returns,
                     ' of these records had abstracts on Lens.org.')
  }
  
  message(report)
  
  return(results)
  
}
