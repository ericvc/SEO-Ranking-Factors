get_text_title <- function(response) {
  response_title = response %>%
    html_nodes("title") %>%
    html_text() %>%
    str_replace_all(., "[\r\n\t]" , " ") %>%
    gsub("rt", "", .) %>% #removes spaces in text strings
    gsub("@\\w+", "", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", "", .) %>% #removes punctuation
    gsub("http\\w+", "", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
    gsub("^ ", "", .) %>% #removes blank at beginning of string
    gsub("^ ", "", .)  %>% #removes blank at end of string
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    #tm::removeWords(., tm::stopwords()) %>%
    tolower # remove all space and new lines
  return(response_title)
}
