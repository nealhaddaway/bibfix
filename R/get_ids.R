#' Get identifiers
#' 
#' @description Retrieve a range of identifiers from different sources 
#' for a record.
#' @param bib_data A bibliographic data set stored as a dataframe, with 
#' each record as a row. Records must contains a column called 'doi' that 
#' contains the DOIs for the records.
#' @return A revised dataframe containing various identifiers for 
#' records that could be found using the APIs.
#' @importFrom roadoi oadoi_fetch
#' @export
#' @examples 
#' \dontrun{
#' bib_data<-revtools::read_bibliography('inst/extdata/export.ris')
#' data <- get_ids(bib_data)
#' }
get_urls <- function(bib_data,
                     source = c('lens')){
  
  #subset that have a doi
  url_query <- subset(bib_data, is.na(doi) == FALSE)
  
  # Get results from lens.org----
  if (grepl(source, 'lens') == TRUE){

    request <- paste0('{
	"query": {
		"terms": {
			"doi": [', paste0('"', paste(url_query$doi, collapse = '", "'), '"'), ']
		}
	},
	"size":500,
	"include": ["external_ids", "lens_id"]
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
    results$magid <- unlist(lapply(results$data.external_ids, function(ch) expss::vlookup('magid', ch, result_column = 'value', lookup_column = 'type')))
    results$coreid <- unlist(lapply(results$data.external_ids, function(ch) expss::vlookup('coreid', ch, result_column = 'value', lookup_column = 'type')))
    results$pmid <- unlist(lapply(results$data.external_ids, function(ch) expss::vlookup('pmid', ch, result_column = 'value', lookup_column = 'type')))
    results$pmcid <- unlist(lapply(results$data.external_ids, function(ch) expss::vlookup('pmcid', ch, result_column = 'value', lookup_column = 'type')))
    
    results <- dplyr::select(results, -c(data.external_ids, results, total))
    results <- dplyr::rename(results, lens_id = data.lens_id)
    
    #join and tidy up the results
    returns_magid <- nrow(subset(results, is.na(results$magid) == FALSE))
    returns_coreid <- nrow(subset(results, is.na(results$coreid) == FALSE))
    returns_pmid <- nrow(subset(results, is.na(results$pmid) == FALSE))
    returns_pmcid <- nrow(subset(results, is.na(results$pmcid) == FALSE))
    returns_lensid <- nrow(subset(results, is.na(results$lens_id) == FALSE))
    results <- merge(x = bib_data, y = results, by.x = 'doi', by.y = 'doi', all.x = TRUE)
    
    #compile report and print to console
    report <- data.frame(identifier = c('magid', 'coreid', 'pmid', 'pmcid', 'lensid', 'QUERIED TOTAL'),
                         returns = c(returns_magid, returns_coreid, returns_pmid, returns_pmcid, returns_lensid, nrow(url_query)))
    
  }
  
  print(report)
  return(results)
  
}
