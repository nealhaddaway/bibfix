#' @title journal_abbreviation
#' @description convert between abbreviated and long forms of journal names based on an updatable dataframe of abbreviations and full names.
#' Support matching where journal cannot be found but there is an almost full match with abbreviated or unabbreviated journal name
#' @param data A dataframe
#' @param journal_names column in the dataframe that contains the titles of the sources
#' @examples 
#' \dontrun{
#' bib_data <- synthesisr::read_refs('inst/extdata/export.ris')
#' bib_data_new <- journal_abbreviation(bib_data, bib_data$journal)
#' }
#' 
journal_abbreviation <- function(data, journal_names) {
  journal_names <- dplyr::enquo(journal_names)
  ref_list <- bibfix::wos_abbrev_table
  ref_list <- ref_list %>%
    dplyr::mutate(source = stringr::str_to_sentence(full))
  data <- data %>%
    dplyr::mutate(source = stringr::str_to_sentence(!!journal_names))

  data <- data %>%
    dplyr::inner_join(., ref_list)
  return(data)
  
## need to add in a fuzzy matching function to output a list of potential matches if there are NAs
## fuzzyjoin takes a looong time to run so I have removed it for now  
}



