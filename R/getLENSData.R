#' Function to query Lens.org
#' 
#' @description Function written by lens.org for use of their API.
#' @param token An access key for the lens.org API. Tokens can be obtained by 
#'   applying for scholarly API access and creating a token once approved. See 
#'   'https://www.lens.org/lens/user/subscriptions#scholar' for further details.
#' @param query A search string formulated according to the Lens.org API 
#'   documentation: 'https://docs.api.lens.org/request-scholar.html'.
#' @return A summary response. The results are viewable using 
#'   'content(data, "text")'. Other details regarding the request (e.g. repsonse 
#'   times) can be accessed through the main output.
#' @importFrom httr add_headers POST
getLENSData <- function(token, query){
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  httr::POST(url = url, httr::add_headers(.headers=headers), body = query)
}
