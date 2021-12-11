#' Remove abstracts from an RIS file
#' 
#' @description Strip out abstracts, which may be copyrighted, from an 
#' RIS file.
#' @param bib_data A bibliographic data set stored as an RIS file.
#' @return A revised RIS file containing records minus abstracts.
#' @importFrom synthesisr read_refs
#' @export
#' @examples 
#' \dontrun{
#' bib_data <- remove_abstracts('inst/extdata/data.csv')
#' }
remove_abstracts <- function(input_filename,
                             save = TRUE,
                             output_filename = 'export',
                             path = NULL){
  
  #read in data as a drataframe
  bib_data <- synthesisr::read_refs(input_filename)
  
  #remove abstracts
  bib_data$abstract <- ''
  
  #convert to ris
  ris <- build_ris(bib_data, save = save, filename = output_filename, path = path)
  
  return(ris)
}
