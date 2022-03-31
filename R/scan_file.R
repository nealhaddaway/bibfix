#' Scan RIS file for damage
#' @examples
#' \dontrun{
#' library(openalex)
#' devtools::install_github("kth-library/openalex", dependencies = TRUE)
#' openalex_polite("neal_haddaway@hotmail.com")
#' library(openalexR)
#' file <- "Users/neal.haddaway/OneDrive - SEI/ESHackathon/4.Remote 2020/citationchaser private/archive/inst/shiny-examples/citationchaser/www/references.ris"
#' refs <- synthesisr::read_refs(file.choose())
#' report <- scan_file(refs)
#' }
scan_file <- function(refs){
  
  #convert all 'NA's to NAs
  refs[refs=="NA"]=NA
  
  #subset relevant columns, ignore the rest
  if(is.null(refs$source_type) == TRUE){refs$source_type <- NA}
  if(is.null(refs$author) == TRUE){refs$author <- NA}
  if(is.null(refs$year) == TRUE){refs$year <- NA}
  if(is.null(refs$title) == TRUE){refs$title <- NA}
  if(is.null(refs$journal) == TRUE){refs$journal <- NA}
  if(is.null(refs$volume) == TRUE){refs$volume <- NA}
  if(is.null(refs$issue) == TRUE){refs$issue <- NA}
  if(is.null(refs$start_page) == TRUE){refs$start_page <- NA}
  if(is.null(refs$end_page) == TRUE){refs$end_page <- NA}
  if(is.null(refs$abstract) == TRUE){refs$abstract <- NA}
  if(is.null(refs$doi) == TRUE){refs$doi <- NA}
  if(is.null(refs$publisher) == TRUE){refs$publisher <- NA}
  refs <- refs[c('source_type', "author", "year", "title", "journal", "volume", "issue", "start_page", "end_page", "abstract", "doi", "publisher")]
  
  #total records
  n_total <- nrow(refs)
  
  #number missing fields
  n_author <- sum(!is.na(refs$author))
  n_year <- sum(!is.na(refs$year))
  n_title <- sum(!is.na(refs$title))
  n_journal <- sum(!is.na(refs$journal))
  n_volume <- sum(!is.na(refs$volume))
  n_issue <- sum(!is.na(refs$issue))
  n_start_page <- sum(!is.na(refs$start_page))
  n_end_page <- sum(!is.na(refs$end_page))
  n_doi <- sum(!is.na(refs$doi))
  n_abstract <- sum(!is.na(refs$abstract))
  
  #number with DOIs
  with_doi <- n_doi
  
  #no doi but with title (findable)
  #create vectors of missing data for each row
  refs$missing_data <- ''
  for(i in 1:nrow(refs)){
    row <- refs[i,]
    missingCols <- paste0(names(refs)[which(is.na(row) == TRUE)], collapse = '; ')
    refs$missing_data[i] <- missingCols
  }
  
  #number complete
  n_complete <- nrow(subset(refs, missing_data ==''))

  #number findable (without DOI but with titles)
  n_findable_titles <- nrow(subset(subset(refs, missing_data != ''), (is.na(doi)==TRUE & is.na(title)==FALSE)))
  
  output <- list(n_complete = n_complete,
                 n_author = n_author,
                 n_year = n_year,
                 n_title = n_title,
                 n_journal = n_journal,
                 n_volume = n_volume,
                 n_issue = n_issue,
                 n_start_page = n_start_page,
                 n_end_page = n_end_page,
                 n_doi = n_doi,
                 n_abstract = n_abstract,
                 n_findable_titles = n_findable_titles,
                 n_total = n_total)
  
  return(output)
  
}
