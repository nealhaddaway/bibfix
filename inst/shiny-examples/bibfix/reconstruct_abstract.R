#' Convert abstract inverted index into full abstract
#' 
#' @description Convert from a inverted index of abstract words into 
#' a full string corresponding to the original abstract. Inverted 
#' index is the way in which an abstract is broken up and provided 
#' by the Open Alex API as a list of words and locations.
#' @param inverted-index A named character vector containing the 
#' locations as integers of words. The character vector can contain 
#' any number of other fields: i.e. it can be the full set of results 
#' returned by the Open Alex API. Each character in the vector 
#' should be named using the Open Alex abstract inverted index term 
#' naming convention as follows: 'abstract_inverted_index_[word]', 
#' where word is the word in question. Multiple occurrences of the 
#' same word are represented by multiple named characters, but with 
#' a numerical suffix (i.e. 'abstract_inverted_index_the1' and 
#' 'abstract_inverted_index_the2').
#' @return A single character string corresponding to the original 
#' abstract.
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
#' reconstruct_abstract(record)
#' }
reconstruct_abstract <- function(inverted_index){
  
  object <- 'abstract_inverted_index'
  data <- data.frame(name = names(inverted_index), value = inverted_index, stringsAsFactors = FALSE)
  values <- data$value
  location <- values[grep(object, data$name)]
  
  words <- grep('abstract_inverted_index', data$name, value = TRUE)
  words <- sub('.*abstract_inverted_index.', '', words)
  words <- sub('.*abstract_inverted_index_', '', words)
  #words <- tm::removeNumbers(words)
  word_location <- data.frame(words = words, location = as.numeric(location), stringsAsFactors = FALSE)
  word_location <- word_location[order(as.numeric(location)),]
  words <- as.character(word_location$words)
  
  new_words <- NULL
  for(i in 1:length(words)){
    if (grepl('[[:alpha:]]+', words[i]) == FALSE){
      new_word <- words[i]
      new_words <- c(new_words, new_word)
    } else {
      new_word <- gsub('[[:digit:]]$', '', words[i])
      new_words <- c(new_words, new_word)
    }
  }
  
  abstract <- paste0(new_words, collapse = ' ')
  abstract <- gsub('_', '.', abstract)
  
  return(abstract)
  
}
