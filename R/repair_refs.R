#' Find empty fields across bibliographic data
#' 
#' @description Fill in missing information for a bibliographic dataset by 
#' searching the Open Alex API.
#' @param refs A data frame outputted from synthesisr.
#' @return A data frame matching the input in style, but containing filled in 
#' information where there were blanks. Where multiple matches were obtained on 
#' Open Alex, these are provided for the user to decide which to retain.
#' @export
#' @examples 
#' \dontrun{
#' library(openalexR)
#' file <- file.choose()
#' refs <- synthesisr::read_refs(file)
#' repaired_refs <- repair_refs(refs, replace_abstracts = TRUE, title_search = FALSE)
#'} 
repair_refs <- function(refs, 
                        replace_abstracts = FALSE,
                        repair_incomplete = TRUE,
                        source = 'lens',
                        title_search = TRUE,
                        token = 'NFxMqRTdXCQRq3uDl8NgduSAXcEf5DAqLIBAPALydHloF0n1n2Xi')
  {
  
  #enter polite pool
  suppressMessages(invisible(capture.output(openalex::openalex_polite("neal_haddaway@hotmail.com"))))
  
  #remove retracted studies
refs<-refs |> 
  filter(isRetracted==0)
  
  
  #create internal id
  refs$intID <- as.numeric(rownames(refs))
  
  #store original data for additional column extraction later
  orig_refs <- refs
  
  ####Tidy and subset####
  #where journal is missing but source is present (e.g. EMBASE RIS) complete journal based on source
  if(is.null(refs$journal)==TRUE && is.null(refs$source) == FALSE){refs$journal<-refs$source}
  
  #if start_page contains start and end pages, split
  if(any(grepl('-', refs$start_page))==TRUE){
    refs$start_page <- str_split_fixed(refs$start_page, '-', 2)[,1]
    refs$end_page <- str_split_fixed(orig_refs$start_page, '-', 2)[,2]
    refs[refs == ''] <- NA
  }
  
  #if vital columns missing, replace with NAs
  if(is.null(refs$source_type) == TRUE){refs$source_type <- NA}
  if(is.null(refs$author) == TRUE){refs$author <- NA}
  if(is.null(refs$year) == TRUE){refs$year <- NA}
  if(is.null(refs$title) == TRUE){refs$title <- NA}
  if(is.null(refs$journal) == TRUE){refs$journal <- NA}
  if(is.null(refs$volume) == TRUE){refs$volume <- NA}
  if(is.null(refs$issue) == TRUE){refs$issue <- NA}
  if(is.null(refs$start_page) == TRUE){refs$start_page <- NA}
  if(is.null(refs$end_page) == TRUE){refs$end_page <- NA}
  if(is.null(refs$abstract) == TRUE){refs$abstract <- NA}
  if(is.null(refs$doi) == TRUE){refs$doi <- NA}
  if(is.null(refs$publisher) == TRUE){refs$publisher <- NA}
  if(is.null(refs$url) == TRUE){refs$url <- NA}
  #remove database result urls held behind paywalls
  if(any(grepl('http://ovidsp.ovid.com/ovidweb', refs$url))==TRUE){refs$url <- NA}
  
  #convert url dois to data only
  refs$doi <- sub('.*.org/', '', refs$doi)
  refs$doi <- decode_dois(refs$doi)
  
  #replace Google Scholar incomplete fields based on presence of ellipsis '…'
  if (repair_incomplete == TRUE){
    refs[] <- lapply(refs, function(x) replace(x, grep("[…]", x), NA))
  }
  
  #if abstracts selected to be replaced, then replace with NA
  if(replace_abstracts == TRUE){refs$abstract <- NA}
  
  #select vital columns for searching
  refs <- refs[c('source_type', "author", "year", "title", "journal", "volume", "issue", "start_page", "end_page", "abstract", "doi", "publisher", "url", "intID")]
  #convert all columns to character strings
  refs <- dplyr::mutate_all(refs, as.character)
  
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
  
  if(nrow(doi_refs)==0){
    doi_refs <- NULL
  } else {
    
    chunks <- function(d, n){      
      chunks <- split(d, ceiling(seq_along(d)/n))
      names(chunks) <- NULL
      return(chunks)
    }
    
    sets <- chunks(doi_refs$doi, 500)
    
    df1 <- data.frame()
    print(paste0('Searching Lens.org for ', nrow(doi_refs), ' DOIs...'))
    for(i in 1:length(sets)){
      request <- paste0('{\n\t"query": {\n\t\t"terms": {\n\t\t\t"','doi','": ["', paste0('', paste(unlist(sets[i]), collapse = '", "'), '"'),']\n\t\t}\n\t},\n\t"size":500\n}')
      data <- getLENSData(token, request) #removed private lens.org API token for GitHub
      record_json <- httr::content(data, "text")
      record_list <- jsonlite::fromJSON(record_json) 
      # report number of input articles returned
      input_number <- record_list[["total"]]
      #print(input_number)
      record_df <- data.frame(record_list)
      df1 <- dplyr::bind_rows(df1, record_df)
    }
    
    #Tidy results df
    #rename result dataframe columns to match inputs
    names(df1) <- sub('data.title', 'title', names(df1))
    names(df1) <- sub('data.abstract', 'abstract', names(df1))
    journaldata <- df1$data.source
    df1$journal <- journaldata$title
    df1$publisher <- journaldata$publisher
    names(df1) <- sub('data.year_published', 'year', names(df1))
    names(df1) <- sub('data.publication_type', 'source_type', names(df1))
    names(df1) <- sub('data.volume', 'volume', names(df1))
    names(df1) <- sub('data.issue', 'issue', names(df1))
    names(df1) <- sub('data.start_page', 'start_page', names(df1))
    names(df1) <- sub('data.end_page', 'end_page', names(df1))
    #extract first URL provided
    df1$url <- unlist(lapply(lapply(df1$data.source_urls, `[[`, 2), paste0, collapse = '; '))
    #extract doi
    df1$doi <- unlist(lapply(df1$data.external_ids, function(ch) maditr::vlookup('doi', ch, result_column = 'value', lookup_column = 'type')))
    
    #collapse authors list
    authors <- list()
    for (i in 1:length(df1$data.authors)) {
      authors <- unlist(c(authors, paste0(df1$data.authors[[i]]$last_name, ', ', 
                                          df1$data.authors[[i]]$first_name, collapse = '; ')))
    }
    df1$author <- authors
    #replace blank authors (', ') with NA
    df1$author[df1$author==", "]=NA
    
    #remove irrelevant columns from df1 results
    df1 <- dplyr::select(df1, c(author, year, title, journal, volume, issue, start_page, end_page, publisher, doi, abstract, source_type, url))
    
    #replace '' with NA
    df1[df1=="NA"]=NA
    
    #records not found
    found <- NULL
    for(i in 1:length(doi_refs$doi)){
      if(any(grepl(tolower(trimws(doi_refs$doi[i])), tolower(df1$doi), fixed=TRUE) == TRUE)){
        found_item <- 'found'
      } else {
        found_item <- 'not found'
      }
      found <- c(found, found_item)
    }
    notFound_dois <- doi_refs$doi[grep('not found', found)]
    
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
    doi_refs <- doi_refs %>%
      dplyr::left_join(df1, by = c("doi")) %>%
      dplyr::mutate(volume = ifelse(is.na(volume.x), volume.y, volume.x)) %>%
      dplyr::select(-c(volume.x, volume.y)) %>%
      dplyr::select(-c(contains('.y')))
    names(doi_refs) <- sub('.x', '', names(doi_refs))
    #lookup issue
    doi_refs <- doi_refs %>%
      dplyr::left_join(df1, by = c("doi")) %>%
      dplyr::mutate(issue = ifelse(is.na(issue.x), issue.y, issue.x)) %>%
      dplyr::select(-c(issue.x, issue.y)) %>%
      dplyr::select(-c(contains('.y')))
    names(doi_refs) <- sub('.x', '', names(doi_refs))
    #lookup start_page
    doi_refs <- doi_refs %>%
      dplyr::left_join(df1, by = c("doi")) %>%
      dplyr::mutate(start_page = ifelse(is.na(start_page.x), start_page.y, start_page.x)) %>%
      dplyr::select(-c(start_page.x, start_page.y)) %>%
      dplyr::select(-c(contains('.y')))
    names(doi_refs) <- sub('.x', '', names(doi_refs))
    #lookup end_page
    doi_refs <- doi_refs %>%
      dplyr::left_join(df1, by = c("doi")) %>%
      dplyr::mutate(end_page = ifelse(is.na(end_page.x), end_page.y, end_page.x)) %>%
      dplyr::select(-c(end_page.x, end_page.y)) %>%
      dplyr::select(-c(contains('.y')))
    names(doi_refs) <- sub('.x', '', names(doi_refs))
    #lookup url
    doi_refs <- doi_refs %>%
      dplyr::left_join(df1, by = c("doi")) %>%
      dplyr::mutate(url = ifelse(is.na(url.x), url.y, url.x)) %>%
      dplyr::select(-c(url.x, url.y)) %>%
      dplyr::select(-c(contains('.y')))
    names(doi_refs) <- sub('.x', '', names(doi_refs))
    
    #reorder columns
    doi_refs <- doi_refs[, c("intID", "source_type", "author", "year", "title", "journal", "volume", "issue", "start_page", "end_page", "abstract", "doi", 
                             "publisher", "url")]
    
    #data frame containing duplicates for selection
    duplicate_dois <- doi_refs[duplicated(doi_refs$doi) | duplicated(doi_refs$doi, fromLast = TRUE), ]
    
    print(paste0('Lens.org search complete: ', nrow(df1), ' DOIs were found on Lens.org.'))
  
    
    
    #if some DOIs weren't found on Lens.org, then search Open Alex
    if (identical(notFound_dois, character(0)) == FALSE){
      #find missing records with DOI on Open Alex
      #search for dois
      doi_input <- data.frame(ids = notFound_dois,
                              type = 'doi')
      
      df1 <- data.frame()
      print(paste0('Searching Open Alex for ', nrow(doi_input), ' DOIs not found on Lens...'))
      for (i in 1:length(doi_input$ids)){
        tryCatch({
          suppressMessages(invisible(capture.output(res <- oa_fetch(identifier = paste0("doi:https://doi.org/", doi_input$ids[i]),entity = "works"))))
          # Only bind if res is not NULL or empty
          # Check if res is a valid list and not empty
          if (!is.null(res) && is.list(res) && length(unlist(res)) != 0) {
            temp_df <- res
            if (nrow(temp_df) > 0) {
              df1 <- dplyr::bind_rows(df1, temp_df)
              #collapse authors list
              authors <- list()
              for (i in 1:length(df1$authors)) {
                authors <- unlist(c(authors, paste0(df1$authors[[i]]$last_name, ', ', 
                                                    df1$authors[[i]]$first_name, collapse = '; ')))
              }
              df1$author <- authors
              #replace blank authors (', ') with NA
              df1$author[df1$author==", "]=NA
              
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
          names(df1) <- sub('volume', 'volume', names(df1))
          names(df1) <- sub('issue', 'issue', names(df1))
          names(df1) <- sub('first_page', 'start_page', names(df1))
          names(df1) <- sub('last_page', 'end_page', names(df1))
          names(df1) <- sub('URL', 'url', names(df1))
          # Safe author assignment
          if ("author" %in% colnames(df1) && length(df1$author) > 0) {
            new_author <- sapply(df1$author, function(a) {
              if (length(a$au_name) > 0) {
                paste(a$au_name, collapse = "; ")
              } else {
                ""
              }  }, USE.NAMES = FALSE)
            df1$author <- new_author
          }
          
          # #collapse authors list
          # new_author <- list()
          # for (i in 1:length(df1$author)) {
          #   new_author <- unlist(c(new_author, paste0(df1$author[[i]]$au_name, collapse = '; ')))
          # }
          # df1$author <- new_author
          
          #remove df$doi URL stem
          df1$doi <- gsub('https://doi.org/', '', df1$doi)
          #remove irrelevant columns from df1 results
          df1 <- dplyr::select(df1, c(author, year, title, journal, volume, issue, start_page, end_page, publisher, doi, abstract, source_type, url))
          
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
          doi_refs <- doi_refs %>%
            dplyr::left_join(df1, by = c("doi")) %>%
            dplyr::mutate(volume = ifelse(is.na(volume.x), volume.y, volume.x)) %>%
            dplyr::select(-c(volume.x, volume.y)) %>%
            dplyr::select(-c(contains('.y')))
          names(doi_refs) <- sub('.x', '', names(doi_refs))
          #lookup issue
          doi_refs <- doi_refs %>%
            dplyr::left_join(df1, by = c("doi")) %>%
            dplyr::mutate(issue = ifelse(is.na(issue.x), issue.y, issue.x)) %>%
            dplyr::select(-c(issue.x, issue.y)) %>%
            dplyr::select(-c(contains('.y')))
          names(doi_refs) <- sub('.x', '', names(doi_refs))
          #lookup start_page
          doi_refs <- doi_refs %>%
            dplyr::left_join(df1, by = c("doi")) %>%
            dplyr::mutate(start_page = ifelse(is.na(start_page.x), start_page.y, start_page.x)) %>%
            dplyr::select(-c(start_page.x, start_page.y)) %>%
            dplyr::select(-c(contains('.y')))
          names(doi_refs) <- sub('.x', '', names(doi_refs))
          #lookup end_page
          doi_refs <- doi_refs %>%
            dplyr::left_join(df1, by = c("doi")) %>%
            dplyr::mutate(end_page = ifelse(is.na(end_page.x), end_page.y, end_page.x)) %>%
            dplyr::select(-c(end_page.x, end_page.y)) %>%
            dplyr::select(-c(contains('.y')))
          names(doi_refs) <- sub('.x', '', names(doi_refs))
          #lookup url
          doi_refs <- doi_refs %>%
            dplyr::left_join(df1, by = c("doi")) %>%
            dplyr::mutate(url = ifelse(is.na(url.x), url.y, url.x)) %>%
            dplyr::select(-c(url.x, url.y)) %>%
            dplyr::select(-c(contains('.y')))
          names(doi_refs) <- sub('.x', '', names(doi_refs))
          
          #reorder columns
          doi_refs <- doi_refs[, c("intID", "source_type", "author", "year", "title", "journal", "volume", "issue", "start_page", "end_page", "abstract", "doi", 
                                   "publisher", "url")]
          
          #data frame containing duplicates for selection
          duplicate_dois <- doi_refs[duplicated(doi_refs$doi) | duplicated(doi_refs$doi, fromLast = TRUE), ]
          
          print(paste0('Open Alex DOI search complete: ', nrow(df1), ' DOIs were found on Open Alex.'))
          
        }, error = function(e) {
          message(paste("Error processing DOI:", doi_input$ids[i], " - ", e$message))
        }, warning = function(w) {
          message(paste("Warning processing DOI:", doi_input$ids[i], " - ", w$message))
        }
        )
          
  }}
  ####End DOI section####
  
  ####Title section####
  #subset records without doi missing information but with a title
  title_refs <- subset(missing_refs, (is.na(doi)==TRUE & is.na(title)==FALSE))
  
  if(title_search == TRUE){
    if(nrow(title_refs)==0){
    title_refs <- NULL
  } else {
    ####Search Open Alex for titles####
    #find missing records with title on Open Alex
    #search on remaining titles
    title_input <- data.frame(ids = title_refs$title,
                              type = 'title')
    
    df2 <- data.frame()
    print(paste0('Searching for ', nrow(title_refs),' titles on Open Alex...'))
    
    for (i in 1:length(title_input$ids)){
      tryCatch({res <- oa_fetch(
        identifier = NULL,
        entity = "works",
        title.search= title_input$ids[i])
      
      if(length(unlist(res))==0){
        new_row <- data.frame(id='not found', TI=title_input$ids[i])
        df2 <- dplyr::bind_rows(df2, new_row)
      } else {
        df2 <- dplyr::bind_rows(df2, oa2df(res, entity = "works"))
      }
    }, error = function(e) {
      message(paste("Error processing Title:", title_input$ids[i], " - ", e$message))
    }, warning = function(w) {
      message(paste("Warning processing title:", title_input$ids[i], " - ", w$message))
    })
    #rename result dataframe columns to match inputs
    names(df2) <- sub('TI', 'title', names(df2))
    names(df2) <- sub('AB', 'abstract', names(df2))
    names(df2) <- sub('SO', 'journal', names(df2))
    names(df2) <- sub('PU', 'publisher', names(df2))
    names(df2) <- sub('PY', 'year', names(df2))
    names(df2) <- sub('DI', 'doi', names(df2))
    names(df2) <- sub('DT', 'source_type', names(df2))
    names(df2) <- sub('volume', 'volume', names(df2))
    names(df2) <- sub('issue', 'issue', names(df2))
    names(df2) <- sub('first_page', 'start_page', names(df2))
    names(df2) <- sub('last_page', 'end_page', names(df2))
    names(df2) <- sub('URL', 'url', names(df2))
    
    #if nothing found, move on
    if(identical(sum(df2 == 'not found'), nrow(df2)) == TRUE){
      title_refs <- NULL
    } else {
      
      #remove df$doi URL stem
      df2$doi <- gsub('https://doi.org/', '', df2$doi)
      #remove irrelevant columns from df2 results
      df2 <- dplyr::select(df2, c(author, year, title, journal, volume, issue, start_page, end_page, publisher, doi, abstract, source_type, url))
      #remove compltete duplicates
      df2 <- df2[!duplicated(df2), ]
      #collapse authors list
      new_author <- list()
      for (i in 1:length(df2$author)) {
        new_author <- unlist(c(new_author, paste0(df2$author[[i]]$au_name, collapse = '; ')))
      }
      df2$author <- new_author
      #replace empty abstracts with NA to avoid replacing in the output
      df2[df2==""]=NA
      
      ####End Search Open Alex for titles####
      
      ####lookup and replace based on titles####
      #lookup missing information
      #lookup authors
      title_refs <- title_refs %>%
        dplyr::left_join(df2, by = c("title")) %>%
        dplyr::mutate(author = ifelse(is.na(author.x), author.y, author.x)) %>%
        dplyr::select(-c(author.x, author.y)) %>%
        dplyr::select(-c(contains('.y')))
      names(title_refs) <- sub('.x', '', names(title_refs))
      title_refs <- title_refs[!duplicated(title_refs), ]
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
      title_refs <- title_refs %>%
        dplyr::left_join(df2, by = c("title")) %>%
        dplyr::mutate(volume = ifelse(is.na(volume.x), volume.y, volume.x)) %>%
        dplyr::select(-c(volume.x, volume.y)) %>%
        dplyr::select(-c(contains('.y')))
      names(title_refs) <- sub('.x', '', names(title_refs))
      title_refs <- title_refs[!duplicated(title_refs), ]
      #lookup issue
      title_refs <- title_refs %>%
        dplyr::left_join(df2, by = c("title")) %>%
        dplyr::mutate(issue = ifelse(is.na(issue.x), issue.y, issue.x)) %>%
        dplyr::select(-c(issue.x, issue.y)) %>%
        dplyr::select(-c(contains('.y')))
      names(title_refs) <- sub('.x', '', names(title_refs))
      title_refs <- title_refs[!duplicated(title_refs), ]
      #lookup start_page
      title_refs <- title_refs %>%
        dplyr::left_join(df2, by = c("title")) %>%
        dplyr::mutate(start_page = ifelse(is.na(start_page.x), start_page.y, start_page.x)) %>%
        dplyr::select(-c(start_page.x, start_page.y)) %>%
        dplyr::select(-c(contains('.y')))
      names(title_refs) <- sub('.x', '', names(title_refs))
      title_refs <- title_refs[!duplicated(title_refs), ]
      #lookup end_page
      title_refs <- title_refs %>%
        dplyr::left_join(df2, by = c("title")) %>%
        dplyr::mutate(end_page = ifelse(is.na(end_page.x), end_page.y, end_page.x)) %>%
        dplyr::select(-c(end_page.x, end_page.y)) %>%
        dplyr::select(-c(contains('.y')))
      names(title_refs) <- sub('.x', '', names(title_refs))
      #lookup url
      title_refs <- title_refs %>%
        dplyr::left_join(df2, by = c("title")) %>%
        dplyr::mutate(url = ifelse(is.na(url.x), url.y, url.x)) %>%
        dplyr::select(-c(url.x, url.y)) %>%
        dplyr::select(-c(contains('.y')))
      names(title_refs) <- sub('.x', '', names(title_refs))
      #remove duplicates
      title_refs <- title_refs[!duplicated(title_refs), ]
      #reorder columns
      title_refs <- title_refs[, c("intID", "source_type", "author", "year", "title", "journal", "volume", "issue", "start_page", "end_page", "abstract", "doi", 
                                   "publisher", "url")]
      #coalesce partial duplicated columns, merging rows sharing partial data
      title_refs <- title_refs %>%
        dplyr::group_by(intID) %>%
        dplyr::summarise_all(coalesce_by_column)
      #data frame containing duplicates for selection
      duplicate_titles <- title_refs[duplicated(title_refs$title) | duplicated(title_refs$title, fromLast = TRUE), ]
      print('Open Alex title search complete.')
    }}}
      
    ####End title section####
  } else {
      title_refs <- NULL
  }
  
  #bind doi lookup and title lookup table with original values not missing information
  result <- dplyr::bind_rows(non_missing, doi_refs, title_refs)
  
  #merge in original columns to supplement returns from OpenAlex
  result$intID <- as.numeric(result$intID)
  
  
  #the folllowing doens't work - it's merging incorrectly
  intermediate <- result %>%
    dplyr::left_join(orig_refs, by = c("intID")) %>%
    dplyr::select(-c(contains('.y')))
  names(intermediate) <- sub('.x', '', names(intermediate))
  
  #bind intermediate output with original references
  intermediate <- bind_rows(intermediate, orig_refs)
  
  #coalesce to gap-fill from original references where there is a missing value in the intermediate output
  output <- intermediate %>%
    dplyr::group_by(intID) %>%
    dplyr::summarise_all(coalesce_by_column)

  return(output)
  }
  


coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}


