#' Returns the header tags from a given URL (h1-h6).
#'
#' @description Returns the text within header tags on a website.
#' @details For a given URL, this function parses the HTML encoding and returnst the text
#' within the header tags levels 1-6.
#' @param Url string. A valid URL to submit a GET request to (example: https://cran.r-project.org/)
#'
#' @import assertthat
#' @import httr
#' @import stringr
#' @import rvest
#' 
#' @examples
#' \dontrun{
#'   url <- "https://cran.r-project.com"
#'   header_tags <- get_header_tags(url)
#'   print(header_tags)
#' }
get_header_tags <- function(Url){
  
  ##check user inputs
  stopifnot(
    not_empty(Url),
    noNA(Url),
    is.string(Url),
    str_starts(Url, pattern = "^(http[s]?://|http[s]?://)")
  )
  
  ##Get response from input URL
  response <- httr::GET(Url)
  
  ##Parse and re-format page content to calculate properties of interest
  if (response$status_code == 200) {
    htags = paste0("h", 1:6)
    the_html <- read_html(response)
    text = sapply(htags, function(htag) {
    the_html %>%
      html_nodes(htag) %>%
      html_text() %>%
      str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
      paste0(., collapse = " ") %>%
      gsub("@\\w+", "", .) %>% #removes spaces in text strings
      gsub("[[:punct:]]", " ", .) %>% #removes punctuation
      gsub("http\\w+", " ", .) %>% #removes links
      gsub("[\n]", " ", .) %>% #replaces tabs with blank space
      gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
      gsub("^ ", " ", .) %>% #removes blank at beginning of string
      gsub("^ ", " ", .)  #%>% #removes blank at end of string
    })
    return(text)
  } else{
    warning("Status code returned was not 200 (return code was ",
            response$status_code,
            ")")
  }
}
