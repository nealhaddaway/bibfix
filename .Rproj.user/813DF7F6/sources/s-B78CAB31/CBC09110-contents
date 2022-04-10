#' Search Open Alex API for records matching different identifiers
#' 
#' @description Search the Open Alex API to bring back records matching 
#' one or more identifiers. Searches based on identifier OR title 
#' (separate query type needed for each).
#' @param input Two column dataframe consisting of one column named 'ids' 
#' contains the identifiers, including 'doi' (digital object identifier), 
#' 'mag' (microsoft academic identifier), 'openalex' (Open Alex 
#' identifier), 'pmid' (PubMed identifier), 'pmcid (PubMed Central 
#' identifier), and 'title' (record title), and one column that contains 
#' the corresponding identifier type for each ID (using those names listed 
#' here).
#' @return a list of character vectors of named reference items returned 
#' from the Open Alex API.
#' @export
#' @examples 
#' \dontrun{
#' devtools::install_github("kth-library/openalex", dependencies = TRUE)
#' library(openalex)
#' openalex_polite("neal_haddaway@hotmail.com")
#' input <- data.frame(ids = c('10.1371/journal.pone.0138237', '10.1186/s13750-016-0059-6', 'The role of tourism and recreation in the spread of non-native species: a systematic review and meta-analysis'),
#'                     type = c('doi', 'doi', 'title'))
#' results <- search_openAlex(input)
#' }
search_openAlex <- function(input){
  
  titles <- subset(input, type == 'title')
  if(nrow(titles)!=0){
    titles_outputs <- list()
    for(i in 1:nrow(titles)){
      result <- list(unlist(openalex::openalex_crawl("works", 
                                                     query = openalex:::openalex_query(filter = paste0("display_name.search:", titles$ids[i])))[[1]]))
      names(result) <- titles$ids[i]
      titles_outputs <- c(titles_outputs, result)
    }
  } else {
    titles_outputs <- NULL
  }
  
  other_ids <- subset(input, type != 'title')
  if(nrow(other_ids)!=0){
    ids_outputs <- list()
    for (i in 1:nrow(other_ids)){
      tryCatch({
        result_df <- openalex::openalex_work(identifier = paste0(other_ids$type[i], ":", other_ids$ids[i]))
        result_list <- as.character(result_df$value)
        names(result_list) <- result_df$name
        result_list <- list(result_list)
        names(result_list) <- other_ids$ids[i]
        ids_outputs <- c(ids_outputs, result_list)
        }, error=function(e){})
    }
  } else {
    ids_outputs <- NULL
  }
  
  outputs <- c(titles_outputs, ids_outputs)
  return(outputs)
  
}


