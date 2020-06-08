get_text_body <- function(response) {
  ##Parse and re-format page content to calculate properties of interest
  response_body = response %>%
    html_nodes("p") %>%
    html_text() %>%
    str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
    paste0(., collapse = " ") %>%
    gsub("\\r\\t", "", .) %>% #removes spaces in text strings
    gsub("@\\w+", "", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", " ", .) %>% #removes punctuation
    gsub("http\\w+", " ", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
    gsub("^ ", " ", .) %>% #removes blank at beginning of string
    gsub("^ ", " ", .)  %>% #removes blank at end of string
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    #tm::removeWords(., tm::stopwords()) %>%
    tolower
  return(response_body)
}
