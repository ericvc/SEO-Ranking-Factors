web_crawler <- function(Url, keyword) {
  ##Get response from input URL
  response = read_html(Url)
  
  ##Evaluate page text properties related to keywords
  response_body = get_text_body(response)
  
  #Proportion of keywords appearing in the page content.
  intext = sapply(str_split(keyword, " "), function(x)
    str_detect(response_body, x))
  propInBody = paste(sum(intext), nrow(intext), sep = "/")
  
  #Conent Length (number of words)
  numWords = response_body %>% str_split(., " ") %>% do.call("c", .) %>% length
  
  #Keywords appearing in the URL
  inurl = sapply(str_split(keyword, " "), function(x)
    str_detect(Url, x))
  propInUrl = paste(sum(inurl), nrow(inurl), sep = "/")
  
  #Is this the domain homepage?
  parse. = url_parse(Url)
  isHomepage = ifelse(parse.$path == "" |
                        parse.$path == "/", "Yes", "No")
  
  ##Parse and re-format page title to calculate properties of interest
  response_title = get_text_title(response)
  
  #How many keywords in the title?
  intitle = sapply(str_split(keyword, " "), function(x)
    str_detect(response_title, x))
  #What proportion of the title are keywords?
  propInTitle = paste(sum(intitle), nrow(intitle), sep = "/")
  
  ##Combine information for output.
  return(rbind(propInTitle, propInBody, propInUrl, numWords, isHomepage) %>%
           as.data.frame)
  
}
