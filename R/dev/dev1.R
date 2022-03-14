refs <- synthesisr::read_refs(file.choose())
names(refs)

author <- paste0(authors, collapse = '; ')
df <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), c('source_type',
                                                          'author',
                                                          'year',
                                                          'title',
                                                          'journal',
                                                          'volume',
                                                          'issue',
                                                          'start_page',
                                                          'end_page',
                                                          'abstract',
                                                          'doi',
                                                          'publisher'))
df_newRow <- data.frame(source_type = if(rlang::is_empty(data$source_type)==TRUE){''}else{data$source_type},
                        author = if(rlang::is_empty(author)==TRUE){''}else{author},
                        year = if(rlang::is_empty(data$year)==TRUE){''}else{data$year},
                        title = if(rlang::is_empty(data$title)==TRUE){''}else{data$title},
                        journal = if(rlang::is_empty(data$journal)==TRUE){''}else{data$journal},
                        volume = if(rlang::is_empty(data$volume)==TRUE){''}else{data$volume},
                        issue = if(rlang::is_empty(data$issue)==TRUE){''}else{data$issue},
                        start_page = if(rlang::is_empty(data$start_page)==TRUE){''}else{data$start_page},
                        end_page = if(rlang::is_empty(data$end_page)==TRUE){''}else{data$end_page},
                        abstract = if(rlang::is_empty(data$abstract)==TRUE){''}else{data$abstract},
                        doi = if(rlang::is_empty(data$doi)==TRUE){''}else{data$doi},
                        publisher = if(rlang::is_empty(data$publisher)==TRUE){''}else{data$publisher})
df <- rbind(df, df_newRow)
