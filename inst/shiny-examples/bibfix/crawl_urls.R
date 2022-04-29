#' Lookup DOI from publisher URL
#'
#' @description Takes a publisher URL for an article, downloads
#' the page code and extracts the first doi URL (i.e. a URL that
#' contains 'doi.org').
#' @param url A publisher or other URL for an article that does
#' not contain a doi within the link.
#' @return A character string corresponding to the DOI.
#' @export
#' @examples \dontrun{
#' url <- 'https://www.sciencedirect.com/science/article/pii/S0048969703000068'
#' doi <- crawl_url(url)
#' }
crawl_url <- function(url){
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  tryCatch(
    {
      message('Searching for DOI...')
      html <- save_html(url, pause = 0)
      code_lines <- unlist(strsplit(html, '\\<div class', useBytes = TRUE))
      y <- grep('.doi.org', code_lines)
      doi <- code_lines[y]
      doi <- str_extract(doi, url_pattern)[1]
      doi <- sub("<.*", "", doi)
      if(grepl('doi', doi)==FALSE){
        doi <- NA
      }
      if(is.null(doi)==TRUE){
        doi <- NA
        message('DOI not found')
      }
      message('...found')
      return(doi)
    }, error=function(cond) {
      message('DOI not found')
      return(NA)
    })
}


#' Lookup DOIs from publisher URLs
#'
#' @description Takes a vector of publisher URLs for an article, downloads
#' the page code and extracts the first doi URLs from each page (i.e. a URL
#' that contains 'doi.org').
#' @param url A vector of strings corresponding to the publisher or other
#' URL for an article that does not contain a doi within the link.
#' @return A vector of character strings corresponding to the DOIs.
#' @export
#' @examples \dontrun{
#' urls <- c('https://www.sciencedirect.com/science/article/pii/S0048969703000068',
#'          'https://www.cambridge.org/core/journals/aquatic-living-resources/article/differences-in-abiotic-water-conditions-between-fluvial-reaches-and-crayfish-fauna-in-some-northern-rivers-of-the-iberian-peninsula/94940FBD4D49B6E36857CD074DA72FB6',
#'          'https://www.publish.csiro.au/MF/MF04111')
#' dois <- crawl_urls(url)
#' }
crawl_urls <- function(urls){
  dois <- lapply(urls, crawl_url)
  return(dois)
}

