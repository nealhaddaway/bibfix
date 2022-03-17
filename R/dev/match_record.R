#' Find matching records from OpenAlex API results list
#' 
#' @description 
#' @param data A list object resulting from a search of the Open 
#' Alex API, containing meta-data for the record.
#' @param object A named item from the list corresponding to the 
#' formatted output items from Open Alex (e.g. 'title').
#' @param preffix If set to TRUE, matches only the preffix of the 
#' item name. Default is FALSE.
#' @return The matching value from the Open Alex results list.
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
#' type <- match_record(record, 'type')
#' }
match_record <- function(data,
                         object,
                         preffix = FALSE){
  #convert data to data frame
  data <- data.frame(name = names(data), value = data, stringsAsFactors = FALSE)
  
  #if preffix is TRUE then just search for name preffix, else look for exact matches
  if(preffix==TRUE){
    results <- as.character(data$value[grep(paste0('^', object), data$name)])
  } else {
    results <- as.character(data$value[grep(paste0('^', object, '$'), data$name)])
  }
  
  return(results)
}
