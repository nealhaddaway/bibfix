#' Get URL links
#' 
#' @description Retrieve URL links to the record in various formats and 
#' from various sources.
#' @param bib_data A bibliographic data set stored as a dataframe, with 
#' each record as a row. Records must contains a column called 'doi' that 
#' contains the DOIs for the records.
#' @param source Source to use for obtaining information. Options are 
#' Unpaywall ('unpaywall', searched via the roadoi package) or The 
#' Lens.org ('lens'). The former is freeand requires only registration 
#' of an email address within the R environment. The Lens.org requires 
#' application for a free, time-limited API token. The ShinyApp includes 
#' token-free access. Other users must request a Lens.org token here: 
#' https://www.lens.org/lens/user/subscriptions#scholar.
#' @param add_GSlink Logical argument specifying whether to generate a 
#' Google Scholar link for the record based on the title. The default 
#' is FALSE.
#' @param convert_doi Logical argument specifying whether to convert the 
#' DOI into a URL by appending 'https://doi.org/'. The default is FALSE.
#' @param add_scihub Logical argument specifying whether to generate a 
#' SciHub link for the record based on the DOI. The default is FALSE.
#' @param scihub_stem Character string for the base URL for SciHub. At 
#' the time of writing, this was 'https://sci-hub.se/' (default value), 
#' but this is likely to move over time, so should be specified here if 
#' a base URL is known to function.
#' @param combine Logical argument to set whether the url outputs 
#' should be combined into a single field (needed for exporting as an 
#' RIS), or whether they should be retained as seperate columns in the 
#' dataset. Default is to combine (combine = TRUE).
#' @return A revised dataframe containing various urls for records that 
#' could be found using the API.
#' @importFrom roadoi oadoi_fetch
#' @importFrom dplyr rename select
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom expss vlookup
#' @importFrom tidyr unite
#' @export
#' @examples 
#' \dontrun{
#' bib_data<-revtools::read_bibliography('inst/extdata/export.ris')
#' data <- get_urls(bib_data, add_GSlink = TRUE, convert_doi = TRUE, add_scihub = TRUE, combine = FALSE)
#' }
get_urls <- function(bib_data,
                     source = 'lens',
                     add_GSlink = FALSE,
                     convert_doi = FALSE,
                     add_scihub = FALSE,
                     scihub_stem = 'https://sci-hub.se/',
                     combine = TRUE){
  
  #if bibliographic data lacks url field, add an empty one
  if (is.element('url', colnames(bib_data)) == FALSE){
    bib_data$url <- NA
  }
  
  #subset records that are missing a url
  missing_url <- subset(bib_data, is.na(url) == TRUE)
  
  #subset that have a doi
  url_query <- subset(missing_url, is.na(doi) == FALSE)
  
  # Get results from Unpaywall via roadoi package----
  if (identical(source, 'unpaywall') == TRUE){
    #reset results
    results <- data.frame(doi = NULL, url_html = NULL, url_pdf = NULL, oa_status = NULL)
    
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
                         url_html = url_best,
                         url_pdf = url_pdf
                         #,oa_status = oa_status
                         )
      results <- rbind(results, data)
    }
    #stop timer
    t1 <- Sys.time()
    #elapsed time
    time <- t1 - t0
    
    #join and tidy up the results
    returns_html <- nrow(subset(results, is.na(url_html) == FALSE))
    returns_pdf <- nrow(subset(results, is.na(url_pdf) == FALSE))
    results <- merge(x = bib_data, y = results, by = "doi", all.x = TRUE)
    
    report <- paste0('Your bibliographic dataset contained ',
                     nrow(missing_url),
                     ' records without URLs, ',
                     nrow(url_query),
                     ' of which had DOIs that could be used to search for URLs on Unpaywall. ',
                     returns_html,
                     ' of these records had URLs corresponding to web locations and ',
                     returns_pdf,
                     ' had URLs corresponding to PDF versions on Unpaywall.',
                     if(combine == TRUE){' The resulting URLs have been condensed into a single field for RIS export.'})
  }
  
  
  # Get results from lens.org----
  if (identical(source, 'lens') == TRUE){

    request <- paste0('{
	"query": {
		"terms": {
			"doi": [', paste0('"', paste(url_query$doi, collapse = '", "'), '"'), ']
		}
	},
	"size":500,
	"scroll": "1m",
	"include": ["external_ids", "source_urls"]
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
    results$url_html <- unlist(lapply(results$data.source_urls, function(ch) {
      if (is.null(expss::vlookup('html', ch, result_column = 'url', lookup_column = 'type')[1]) == TRUE){
        return(NA)
      } else {expss::vlookup('html', ch, result_column = 'url', lookup_column = 'type')[1]}
    }))
    results$url_pdf <- unlist(lapply(results$data.source_urls, function(ch) {
      if (is.null(expss::vlookup('pdf', ch, result_column = 'url', lookup_column = 'type')[1]) == TRUE){
        return(NA)
      } else {expss::vlookup('pdf', ch, result_column = 'url', lookup_column = 'type')[1]}
    }))
    results <- data.frame(url_html = results$url_html, url_pdf = results$url_pdf, doi = results$doi)
    
    #join and tidy up the results
    returns_html <- nrow(subset(results, is.na(results$url_html) == FALSE))
    returns_pdf <- nrow(subset(results, is.na(results$url_pdf) == FALSE))
    results <- merge(x = bib_data, y = results, by.x = 'doi', by.y = 'doi', all.x = TRUE)
    
    #compile report and print to console
    report <- paste0('Your bibliographic dataset contained ',
                     nrow(missing_url),
                     ' records without URLs, ',
                     nrow(url_query),
                     ' of which had DOIs that could be used to search for URLs on Lens.org. ',
                     returns_html,
                     ' of these records had URLs corresponding to web locations and ',
                     returns_pdf,
                     ' had URLs corresponding to PDF versions on Lens.org.',
                     if(combine == TRUE){' The resulting URLs have been condensed into a single field for RIS export.'})
    
  }
  
  # Add in Google Scholar link to title search result----
  if (add_GSlink == TRUE){
    results$url_GS <- sub(" ", "", paste0("https://scholar.google.co.uk/scholar?start=0&q=", gsub(" ", "+", (gsub("[[:punct:]]", "", results$title))), "&hl=en&as_sdt=0,5"))
  }
  
  # Add in SciHub link to title search result----
  if (add_scihub == TRUE){
    results$url_scihub <- paste0(scihub_stem, results$doi)
    results$url_scihub <- gsub(paste0(scihub_stem, 'NA'), '', results$url_scihub)
  }
  
  # Convert doi into url
  if (convert_doi == TRUE){
    results$url_doi <- paste0('https://doi.org/', results$doi)
    results$url_doi <- gsub('https://doi.org/NA', '', results$url_doi)
  }

  #if combine = TRUE, combine fields
  if (combine == TRUE){
    #identify all columns starting with 'url'
    url_cols <- results[,grep("url", names(results), value=TRUE)]
    #combine these columns into a single column
    new_url <- tidyr::unite(url_cols, url, sep = ' ')
    new_url <- sapply(new_url, function(x) {gsub(' ', '; ', trimws(gsub('  ', ' ',(gsub('NA', '', x)))))})
    #drop all url columns
    results <- dplyr::select(results, -contains("url"))
    #add the new url column back in
    results$url <- new_url[,1]
  }
  
  print(report)
  return(results)
  
}
