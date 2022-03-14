#' Find empty fields across bibliographic data
#' 
#' @description 
#' @param refs A dataframe outputted from synthesisr.
#' @return 
#' @export
#' @examples 
#' \dontrun{
#' file <- "/Users/neal.haddaway/OneDrive - SEI/ESHackathon/4.Remote 2020/citationchaser private/archive/inst/shiny-examples/citationchaser/www/references.ris"
#' refs <- synthesisr::read_refs(file)
#' refs <- refs[,1:12] #abstracts split incorrectly so manually remove them for this input file
#' }

#create internal id
refs$intID <- paste0('refs', rownames(refs))

#convert all 'NA's to NAs
refs[refs=="NA"]=NA

#create vectors of missing data for each row
refs$missing_data <- ''
for(i in 1:nrow(refs)){
  row <- refs[i,]
  missingCols <- paste0(names(refs)[which(is.na(row) == TRUE)], collapse = '; ')
  refs$missing_data[i] <- missingCols
}

#subset records with a DOI (easier to search)
doi_refs <- subset(refs, is.na(doi)==FALSE)

#report section
doi_missingData <- subset(doi_refs, missing_data != '')

input <- data.frame(ids = doi_missingData$doi, type = 'doi')
results <- search_openAlex(input)
names(results)

doi_missingData$found <- doi_missingData$doi %in% names(results)

no_abstract <- subset(doi_missingData, grepl('abstract', missing_data)==TRUE)


