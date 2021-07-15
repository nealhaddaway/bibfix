#' Build RIS files from other sources
#' 
#' @description Builds an RIS file based on a basic input of 
#' fields corresponding to a minimum information for 
#' deduplication and record identification from external 
#' API sources (e.g. CrossRef). 
#' @param input A dataframe object containing bibliographic 
#' data. Each item is an independent line in the 
#' dataframe. The dataframe must contain 
#' columns named as follows: 'authors', 'year', 'title', 
#' 'source', 'volume', 'issue', 'start_page', 'end_page', 
#' and 'doi'.
#' @param path Path to which file should be saved.
#' @return An RIS formatted text file saved to the desired 
#' path.
#' @export
#' @examples 
#' \dontrun{
#' data <- read.csv('inst/extdata/data.csv')
#' build_ris(data)
#' }
build_ris <- function(data,
                      path = NULL){
  
  if (is.data.frame(data) == FALSE){
    stop('Please ensure the input object is a data frame')
  }
  
  #create RIS file
  ris <- paste(paste0('\n',
                      'TY  - ', data$type, '\n',
                      'AU  - ', data$authors, '\n',
                      'TI  - ', data$title, '\n',
                      'PY  - ', data$year, '\n',
                      'AB  - ', data$abstract, '\n',
                      'SP  - ', data$start_page, '\n',
                      'EP  - ', data$end_page, '\n',
                      'JF  - ', data$source, '\n',
                      'VL  - ', data$volume, '\n',
                      'IS  - ', data$issue, '\n',
                      'DO  - ', data$doi, '\n',
                      'UR  - ', data$url, '\n',
                      'ER  - '),
               collapse = '\n')
  
  write.table(ris, file = paste0(path, "export.ris"), sep = "")
  
}
