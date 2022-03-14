#' Read in RIS file
#' 
#' @description Read in references from an RIS file.
#' @param file File path.
#' @export
#' @examples 
#' \dontrun{
#' refs <- file.choose()
#' read_RIS(refs)
#' }
read_RIS <- function(file){
  
  #read in file as a single text string
  text <- readr::read_file(file)
  
  #split text by starting 'TY  -' tag
  refItems <- paste0('TY  - ', unlist(strsplit(text, 'TY  -', fixed=T)))
  refItems <- refItems[2:length(refItems)]
  
  refs_df <- data.frame()
  for (i in 1:length(refItems)){
    recordItems <- gsub('\r', '', refItems[i])
    recordItems <- unlist(strsplit(recordItems, '\n', fixed=T))
    recordItems <- recordItems[!recordItems == '']
    names <- substr(recordItems, 1, 2)
    recordItems <- mapply(substring, recordItems, 7)
    record_df <- data.frame(recordItems, row.names = names)
    df_t <- data.table::transpose(record_df)
    rownames(df_t) <- NULL
    colnames(df_t) <- rownames(record_df)
    refs_df <- dplyr::bind_rows(refs_df, df_t)
  }
  
  
}


## not working where carriage returns exist inside abstract... need to split some other way or recombine abstract first

grepl('[:upper:]', recordItems)
abstract <- recordItems[grep('AB  - ', recordItems)]
