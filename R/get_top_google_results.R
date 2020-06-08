#' Get the top results from Google's search engine results page
#'
#' @description This function pulls the title and URL of the top-ranked web pages
#' from a given Google search query, and returns the information in the form of a data.frame object.
#' @details The function operates by constructing a URL from the provided query and desiered number of results.
#' The results of a GET request to the URL are then parsed to retrieve the titles and URLs of ranking 
#' web pages. If that the status code from the GET request is not '200', no results are returned.
#'
#' @param query string. A character string representing the query to be passed to the Google Search Engine 
#' @param num_results integer. The maximum number of results to return from the search. 
#' @param platform string. The analysis strategy to use. Options: "desktop" or
#'     "mobile". Defaults to "desktop"
#' @param headers string. Custom request headers to use during the search. Overrides the arguments to 'platform'.
#'
#' @return 'tibble' data frame with two columns representing the page title ('Title') and associated URL ('URL').
#'
#' @import assertthat
#' @import rvest
#' @import xml2
#' @import stringr
#' @import httr
#' @import tidyr
#' 
#' @examples
#' \dontrun{
#' results <- get_top_google_results("CRAN")
#' }
get_top_google_results <- function(query, num_results=20, platform="desktop", user_headers=NULL){
  
  #check function inputs
  stopifnot(
    not_empty(query) & noNA(query),
    not_empty(num_results) & num_results == as.integer(num_results),
    not_empty(platform) & !is.null(platform) & platform %in% c("mobile","desktop")
  )
  
  query. <- query %>%
    str_trim() %>%
    str_replace_all(pattern="\\s{1,}", replacement = "+")
  
  if(is.null(user_headers)){
    if(platform == "desktop"){
      headers = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:65.0) Gecko/20100101 Firefox/65.0"
    } else{
      # mobile user-agent
      headers = "Mozilla/5.0 (Linux; Android 7.0; SM-G930V Build/NRD90M) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.125 Mobile Safari/537.36"
    }
  } else{
    headers = user_headers
  }
  
  Url <-  paste0("https://www.google.com/search?q=",query.,"&num=",num_results)
  
  response <- httr::GET(Url, config = add_headers("user-agent"=headers))
  
  if(response$status_code == 200){
    
    pageTitle <- response %>%
      read_html %>%
      html_nodes("div[class='r']") %>%
      html_nodes("h3") %>%
      html_text()
    
    pageLinks <- response %>%
      read_html %>%
      html_nodes("div[class='r']") %>%
      html_node("a") %>%
      html_attr("href")
    
    #pageLinks <- pageLinks[pageLinks!="#"
    data <- as_tibble(cbind(pageTitle,pageLinks))
    names(data) <- c("Title","URL")
    return(data)
    
  } else{
    warning("Status code returned was not 200 (return code was ",
            response$status_code,
            ")")
  }
  
}
