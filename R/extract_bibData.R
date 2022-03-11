#' Format Open Alex API outputs into reference items
#' 
#' @description Open Alex outputs are formatted into a list of named items 
#' needed to create bibliographic files and other content. 
#' @param 
#' @return A list of named items.
#' @export
#' @examples
#' \dontrun{
#' devtools::install_github("kth-library/openalex", dependencies = TRUE)
#' library(openalex)
#' openalex_polite("neal_haddaway@hotmail.com")
#' input <- data.frame(ids = c('10.1371/journal.pone.0138237', '10.1186/s13750-016-0059-6', 'The role of tourism and recreation in the spread of non-native species: a systematic review and meta-analysis'),
#'                     type = c('doi', 'doi', 'title'))
#' results <- search_openAlex(input)
#' record <- results[['10.1371/journal.pone.0138237']]
#' data <- extract_bibData(record)
#' }
extract_bibData <- function(record){
  
  type <- match_record(record, 'type')
  authors <- match_record(record, 'authorships.author.display_name')
  author_orcids <- match_record(record, 'authorships.author.orcid')
  author_addresses <- match_record(record, 'authorships.raw_affiliation_string')
  year <- match_record(record, 'publication_year')
  title <- match_record(record, 'title')
  journal <- match_record(record, 'host_venue.display_name')
  volume <- match_record(record, 'biblio.volume')
  issue <- match_record(record, 'biblio.issue')
  start_page <- match_record(record, 'biblio.first_page')
  end_page <- match_record(record, 'biblio.last_page')
  abstract <- reconstruct_abstract(record)
  doi <- match_record(record, 'doi')
  magid <- match_record(record, 'ids.mag')
  pmid <- match_record(record, 'ids.pmid')
  pmcid <- match_record(record, 'ids.pmcid')
  openalex_id <- match_record(record, 'id')
  is_retracted <- match_record(record, 'is_retracted')
  references <- match_record(record, 'referenced_work', preffix=TRUE)
  publisher <- match_record(record, 'host_venue.publisher')
  publication_date <- match_record(record, 'publication_date')
  concepts <- match_record(record, 'concepts.display_name')
  urls <- c(match_record(record, 'open_access.oa_url'),
            match_record(record, 'host_venue.url'),
            match_record(record, 'alternate_host_venues.url'))
  related_works <- match_record(record, 'related_works', preffix=TRUE)
  is_oa <- match_record(record, 'open_access.is_oa')
  oa_status <- match_record(record, 'open_access.oa_status')
  license <- match_record(record, 'host_venue.license')
  oa_url <- match_record(record, 'open_access.oa_url')
  version <- match_record(record, 'host_venue.version')
  citations <- jsonlite::fromJSON(htm2txt::gettxt('https://api.openalex.org/works?filter=cites:W2147544664&per-page=200'))[["results"]][["id"]] #this brings back up to 200 citations, cursor based pagination not yet working
  
  output <- list(type = type,
                 authors = authors,
                 author_orcids = author_orcids,
                 author_addresses = author_addresses,
                 year = year,
                 title = title,
                 journal = journal,
                 volume = volume,
                 issue = issue,
                 start_page = start_page,
                 end_page = end_page,
                 abstract = abstract,
                 doi = doi,
                 magid = magid,
                 pmid = pmid,
                 pmcid = pmcid,
                 openalex_id = openalex_id,
                 is_retracted = is_retracted,
                 references = references,
                 publisher = publisher,
                 publication_date = publication_date,
                 concepts = concepts,
                 urls = urls,
                 related_works = related_works,
                 is_oa = is_oa,
                 oa_status = oa_status,
                 license = license,
                 oa_url = oa_url,
                 version = version,
                 citations = citations)
  
  return(output)
  
}

