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
#' @param save Logical argument to specify whether the output 
#' file should be saved as an .ris file.
#' @param filename Optional name of the output file if 
#' save = TRUE. Default is 'export'.
#' @param path Path to which file should be saved.
#' @return An RIS formatted text file saved to the desired 
#' path.
#' @export
#' @examples 
#' \dontrun{
#' data <- read.csv('inst/extdata/data.csv')
#' ris <- build_ris(data, save=TRUE)
#' }
build_ris <- function(data,
                      save = FALSE,
                      filename = 'export',
                      path = NULL){
  
  if (is.data.frame(data) == FALSE){
    stop('Please ensure the input object is a data frame')
  }
  
  #create RIS file
  ris <- paste(paste0('\n',
                      'TY  - ', data$source_type, '\n',
                      'AU  - ', data$author, '\n',
                      'TI  - ', data$title, '\n',
                      'PY  - ', data$year, '\n',
                      'AB  - ', data$abstract, '\n',
                      'SP  - ', data$start_page, '\n',
                      'EP  - ', data$end_page, '\n',
                      'JF  - ', data$journal, '\n',
                      'VL  - ', data$volume, '\n',
                      'IS  - ', data$issue, '\n',
                      'DO  - ', data$doi, '\n',
                    # 'UR  - ', data$url, '\n',
                      'PB  - ', data$publisher, '\n',
                      'ER  - '),
               collapse = '\n')
  
  #generate report if file exported as .ris
  if (save == TRUE){
    write.table(ris, file = paste0(path, filename, '.ris'), row.names = FALSE, col.names = FALSE)
    if (is.null(path) == TRUE){
      location <- 'your working directory'
    } else {
      location <- paste0(path)
    }
    report <- paste0('The output file "', filename, '.ris" has been saved to ', location)
    message(report)
  }
  
  return(ris)
  
}
