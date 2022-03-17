#' Find empty fields across bibliographic data
#' 
#' @description Fill in missing information for a bibliuographic dataset by 
#' searching the Open Alex API.
#' @param refs A data frame outputted from synthesisr.
#' @return A data frame matching the input in style, but containing filled in 
#' information where there were blanks. Where multiple matches were obtained on 
#' Open Alex, these are provided for the user to decide which to retain.
#' @export
#' @examples 
#' \dontrun{
#' library(openalex)
#' devtools::install_github("kth-library/openalex", dependencies = TRUE)
#' openalex_polite("neal_haddaway@hotmail.com")
#' library(openalexR)
#' file <- "Users/neal.haddaway/OneDrive - SEI/ESHackathon/4.Remote 2020/citationchaser private/archive/inst/shiny-examples/citationchaser/www/references.ris"
#' refs <- synthesisr::read_refs(file.choose())
#' refs <- refs[,1:12] #abstracts split incorrectly so manually remove them for this input file
#' repaired refs <- repair_refs(refs)
#' }
repair_refs <- function(refs){
  
  ####Tidy and subset####
  #create internal id
  refs$intID <- as.numeric(rownames(refs))
  
  #convert all 'NA's to NAs
  refs[refs=="NA"]=NA
  
  #subset records missing data
  #create vectors of missing data for each row
  refs$missing_data <- ''
  for(i in 1:nrow(refs)){
    row <- refs[i,]
    missingCols <- paste0(names(refs)[which(is.na(row) == TRUE)], collapse = '; ')
    refs$missing_data[i] <- missingCols
  }
  #subset based on missing data
  missing_refs <- subset(refs, missing_data != '')
  non_missing <- subset(refs, missing_data == '')
  missing_refs <- dplyr::select(missing_refs, -c(missing_data))
  non_missing <- dplyr::select(non_missing, -c(missing_data))
  refs <- dplyr::select(refs, -c(missing_data))
  ####End Tidy and subset####
  
  ####DOI section####
  #subset records with a DOI (easier to search)
  doi_refs <- missing_refs[!is.na(missing_refs$doi),]
  
  #find missing records with DOI on Open Alex
  #search for dois
  doi_input <- data.frame(ids = doi_refs$doi,
                          type = 'doi')

  df1 <- data.frame()
  for (i in 1:length(doi_input$ids)){
    query_work <- oaQueryBuild(
      identifier = paste0("doi:https://doi.org/", doi_input$ids[i]),
      entity = "works"
    )
    res <- oaApiRequest(
      query_url = query_work
    )
    if(length(unlist(res))==0){
      df1 <- df1
    } else {
      df1 <- dplyr::bind_rows(df1, oa2df(res, entity = "works"))
    }
  }
  
  #Tidy results df
  #rename result dataframe columns to match inputs
  names(df1) <- sub('TI', 'title', names(df1))
  names(df1) <- sub('AB', 'abstract', names(df1))
  names(df1) <- sub('SO', 'journal', names(df1))
  names(df1) <- sub('PU', 'publisher', names(df1))
  names(df1) <- sub('PY', 'year', names(df1))
  names(df1) <- sub('DI', 'doi', names(df1))
  names(df1) <- sub('DT', 'source_type', names(df1))
  #remove df$doi URL stem
  df1$doi <- gsub('https://doi.org/', '', df1$doi)
  #remove irrelevant columns from df1 results
  df1 <- dplyr::select(df1, c(author, year, title, journal, publisher, doi, abstract, source_type))
  #collapse authors list
  author <- list()
  for (i in 1:length(df1$author)) {
    author <- unlist(c(author, paste0(df1$author[[i]]$au_name, collapse = '; ')))
  }
  df1$author <- author
  
  #lookup missing information
  #lookup authors
  doi_refs <- doi_refs %>%
    dplyr::left_join(df1, by = c("doi")) %>%
    dplyr::mutate(author = ifelse(is.na(author.x), author.y, author.x)) %>%
    dplyr::select(-c(author.x, author.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(doi_refs) <- sub('.x', '', names(doi_refs))
  #lookup title
  doi_refs <- doi_refs %>%
    dplyr::left_join(df1, by = c("doi")) %>%
    dplyr::mutate(title = ifelse(is.na(title.x), title.y, title.x)) %>%
    dplyr::select(-c(title.x, title.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(doi_refs) <- sub('.x', '', names(doi_refs))
  #lookup abstract
  doi_refs <- doi_refs %>%
    dplyr::left_join(df1, by = c("doi")) %>%
    dplyr::mutate(abstract = ifelse(is.na(abstract.x), abstract.y, abstract.x)) %>%
    dplyr::select(-c(abstract.x, abstract.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(doi_refs) <- sub('.x', '', names(doi_refs))
  #lookup journal
  doi_refs <- doi_refs %>%
    dplyr::left_join(df1, by = c("doi")) %>%
    dplyr::mutate(journal = ifelse(is.na(journal.x), journal.y, journal.x)) %>%
    dplyr::select(-c(journal.x, journal.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(doi_refs) <- sub('.x', '', names(doi_refs))
  #lookup publisher
  doi_refs <- doi_refs %>%
    dplyr::left_join(df1, by = c("doi")) %>%
    dplyr::mutate(publisher = ifelse(is.na(publisher.x), publisher.y, publisher.x)) %>%
    dplyr::select(-c(publisher.x, publisher.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(doi_refs) <- sub('.x', '', names(doi_refs))
  #lookup volume
  
  #lookup issue
  
  #lookup start_page
  
  #lookup end_page
  
  #reorder columns
  doi_refs <- doi_refs[, c("intID", "source_type", "author", "year", "title", "journal", "volume", "issue", "start_page", "end_page", "abstract", "doi", 
                           "publisher")]
  
  #data frame containing duplicates for selection
  duplicate_dois <- doi_refs[duplicated(doi_refs$doi) | duplicated(doi_refs$doi, fromLast = TRUE), ]
  
  ####End DOI section####
  
  ####Title section####
  #subset records without doi missing information but with a title
  title_refs <- subset(missing_refs, (is.na(doi)==TRUE & is.na(title)==FALSE))
  
  ####Search Open Alex for titles####
  #find missing records with title on Open Alex
  #search on remaining titles
  title_input <- data.frame(ids = title_refs$title,
                            type = 'title')
  
  df2 <- data.frame()
  for (i in 1:length(title_input$ids)){
    query_work <- oaQueryBuild(
      identifier = NULL,
      entity = "works",
      filter=paste0('title.search:', title_input$ids[i]),
      search=NULL,
      sort="relevance_score:desc",
      endpoint = "https://api.openalex.org/"
    )
    res <- oaApiRequest(
      query_url = query_work
    )
    if(length(unlist(res))==0){
      new_row <- data.frame(id='not found', TI=title_input$ids[i])
      df2 <- dplyr::bind_rows(df2, new_row)
    } else {
      df2 <- dplyr::bind_rows(df2, oa2df(res, entity = "works"))
    }
  }
  #rename result dataframe columns to match inputs
  names(df2) <- sub('TI', 'title', names(df2))
  names(df2) <- sub('AB', 'abstract', names(df2))
  names(df2) <- sub('SO', 'journal', names(df2))
  names(df2) <- sub('PU', 'publisher', names(df2))
  names(df2) <- sub('PY', 'year', names(df2))
  names(df2) <- sub('DI', 'doi', names(df2))
  names(df2) <- sub('DT', 'source_type', names(df2))
  #remove df$doi URL stem
  df2$doi <- gsub('https://doi.org/', '', df2$doi)
  #remove irrelevant columns from df1 results
  df2 <- dplyr::select(df2, c(author, year, title, journal, publisher, doi, abstract, source_type))
  #remove compltete duplicates
  df2 <- df2[!duplicated(df2), ]
  #collapse authors list
  author <- list()
  for (i in 1:length(df2$author)) {
    author <- unlist(c(author, paste0(df2$author[[i]]$au_name, collapse = '; ')))
  }
  df2$author <- author
  
  ####End Search Open Alex for titles####
  
  ####lookup and replace based on titles####
  #lookup missing information
  #lookup authors
  title_refs <- title_refs %>%
    dplyr::left_join(df2, by = c("doi")) %>%
    dplyr::mutate(authors = ifelse(is.na(author.x), author.y, author.x)) %>%
    dplyr::select(-c(author.x, author.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(title_refs) <- sub('.x', '', names(title_refs))
  #lookup doi
  title_refs <- title_refs %>%
    dplyr::left_join(df2, by = c("title")) %>%
    dplyr::mutate(doi = ifelse(is.na(doi.x), doi.y, doi.x)) %>%
    dplyr::select(-c(doi.x, doi.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(title_refs) <- sub('.x', '', names(title_refs))
  title_refs <- title_refs[!duplicated(title_refs), ]
  #lookup abstract
  title_refs <- title_refs %>%
    dplyr::left_join(df2, by = c("title")) %>%
    dplyr::mutate(abstract = ifelse(is.na(abstract.x), abstract.y, abstract.x)) %>%
    dplyr::select(-c(abstract.x, abstract.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(title_refs) <- sub('.x', '', names(title_refs))
  title_refs <- title_refs[!duplicated(title_refs), ]
  #lookup journal
  title_refs <- title_refs %>%
    dplyr::left_join(df2, by = c("title")) %>%
    dplyr::mutate(journal = ifelse(is.na(journal.x), journal.y, journal.x)) %>%
    dplyr::select(-c(journal.x, journal.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(title_refs) <- sub('.x', '', names(title_refs))
  title_refs <- title_refs[!duplicated(title_refs), ]
  #lookup publisher
  title_refs <- title_refs %>%
    dplyr::left_join(df2, by = c("title")) %>%
    dplyr::mutate(publisher = ifelse(is.na(publisher.x), publisher.y, publisher.x)) %>%
    dplyr::select(-c(publisher.x, publisher.y)) %>%
    dplyr::select(-c(contains('.y')))
  names(title_refs) <- sub('.x', '', names(title_refs))
  title_refs <- title_refs[!duplicated(title_refs), ]
  #lookup volume
  
  #lookup issue
  
  #lookup start_page
  
  #lookup end_page
  
  #reorder columns
  title_refs <- title_refs[, c("intID", "source_type", "author", "year", "title", "journal", "volume", "issue", "start_page", "end_page", "abstract", "doi", 
                               "publisher")]
  #data frame containing duplicates for selection
  duplicate_titles <- title_refs[duplicated(title_refs$title) | duplicated(title_refs$title, fromLast = TRUE), ]
  ####End title section####
  
  #bind doi lookup and title lookup table with original values not missing information
  output <- dplyr::bind_rows(non_missing, doi_refs, title_refs)
  
  return(output)
  
}





