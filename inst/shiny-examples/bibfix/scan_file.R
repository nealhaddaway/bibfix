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
#' 

library(synthesisr)
library(tidyverse)

scan_file <- function(refs) {
  
  # Convert all 'NA's to NAs
  refs[refs == "NA"] <- NA
  
  # Ensure required columns exist
  required_cols <- c('source_type', 'author', 'year', 'title', 'journal', 
                     'volume', 'issue', 'start_page', 'end_page', 
                     'abstract', 'doi', 'publisher')
  for (col in required_cols) {
    if (is.null(refs[[col]])) refs[[col]] <- NA
  }
  
  # Subset relevant columns
  refs <- refs[required_cols]
  
  # Total records
  n_total <- nrow(refs)
  
  # Count missing fields
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
  
  # Count complete records
  refs$missing_data <- apply(refs, 1, function(row) {
    paste0(names(refs)[is.na(row)], collapse = '; ')
  })
  n_complete <- sum(refs$missing_data == '')
  
  # Count findable records (without DOI but with title)
  n_findable_titles <- sum(is.na(refs$doi) & !is.na(refs$title))
  
  # Search for "retracted"
  
 # retracted<-read.csv("https://api.labs.crossref.org/data/retractionwatch?name@email.org")
 
 retracted<-read.csv("https://raw.githubusercontent.com/Mark-Eng/bibfix/refs/heads/master/data/retraction_watch.csv")
  
refs<-refs |> 
    mutate(isRetracted=if_else(doi %in% c(retracted_test$OriginalPaperDOI, retracted_test$RetractionDOI), 1, 0))
  
n_retracted<-sum(refs$isRetracted)  

  
  # Return results
  output <- list(
    refs=refs,
    n_complete = n_complete,
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
    n_retracted = n_retracted,
    n_total = n_total
  )
  
  return(output)
}
