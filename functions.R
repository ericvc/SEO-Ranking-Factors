get_H_tags <- function(Url){
  
  response = read_html(Url)
  
  ##Parse and re-format page content to calculate properties of interest
  tags = sapply(paste0("h",1:6), function(htag){
  response %>%
    html_nodes(htag) %>%
    html_text() %>%
    str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
    paste0(., collapse = " ") %>%
    gsub("rt","", .) %>% #removes spaces in text strings
    gsub("@\\w+","", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", " ", .) %>% #removes punctuation
    gsub("http\\w+", " ", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
    gsub("^ ", " ", .) %>% #removes blank at beginning of string
    gsub("^ ", " ", .)  #%>% #removes blank at end of string
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    #tm::removeWords(., tm::stopwords()) %>%
    #tolower
  })
  
  return(tags)
}

WebCrawler <- function(Url, keyword){
  
  response = read_html(Url)

  ##Parse and re-format page content to calculate properties of interest
  response_body = response %>%
    html_nodes("body") %>%
    html_text() %>%
    str_replace_all(., "[\r\n\t]" , " ") %>% # remove all space and new lines
    paste0(., collapse = " ") %>%
    gsub("rt","", .) %>% #removes spaces in text strings
    gsub("@\\w+","", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", " ", .) %>% #removes punctuation
    gsub("http\\w+", " ", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
    gsub("^ ", " ", .) %>% #removes blank at beginning of string
    gsub("^ ", " ", .)  %>% #removes blank at end of string
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    #tm::removeWords(., tm::stopwords()) %>%
    tolower 
  
  #Proportion of keywords appearing in the page content.
  intext = sapply(str_split(keyword," "), function(x) str_detect(response_body, x))
  propInBody = paste(sum(intext),nrow(intext),sep="/")

  #Conent Length (number of words)
  numWords = response_body %>% str_split(., " ") %>% do.call("c",.) %>% length
  
  #Keywords appearing in the URL
  inurl = sapply(str_split(keyword," "), function(x) str_detect(Url, x))
  propInUrl = paste(sum(inurl), nrow(inurl), sep="/")
  
  #Is this the domain homepage
  parse. = url_parse(Url)
  isHomepage = ifelse(parse.$path == "" | parse.$path=="/", "Yes","No")

  ##Parse and re-format page title to calculate properties of interest
  response_title = response %>%
    html_nodes("title") %>%
    html_text() %>%
    str_replace_all(., "[\r\n\t]" , " ") %>%
    gsub("rt","", .) %>% #removes spaces in text strings
    gsub("@\\w+","", .) %>% #removes spaces in text strings
    gsub("[[:punct:]]", "", .) %>% #removes punctuation
    gsub("http\\w+", "", .) %>% #removes links
    gsub("[\n]", " ", .) %>% #replaces tabs with blank space
    gsub("[ |\t]{2,}", " ", .) %>% #replaces tabs with blank space
    gsub("^ ", "", .) %>% #removes blank at beginning of string
    gsub("^ ", "", .)  %>% #removes blank at end of string
    #remove "stop words" - common words in the English language providing little context (a, an, the, or, etc.)
    #tm::removeWords(., tm::stopwords()) %>%
    tolower # remove all space and new lines
  intitle = sapply(str_split(keyword," "), function(x) str_detect(response_title, x))
  propInTitle = paste(sum(intitle),nrow(intitle), sep="/")
  
  rbind(propInTitle, propInBody, propInUrl, numWords, isHomepage) %>% as.data.frame
  
}

SEMRush_API_keyword_overview <- function(keyword){
  kw = str_replace_all(keyword, " ","+") #format keyword string
  str1 = "https://api.semrush.com/?type=phrase_this&key=9ef54d3c4f26358e21754e419e1574ba&phrase=" #SEMRush API link
  str2 = "&export_columns=Nq,Nr,Cp&database=us"
  Url = paste0(str1,kw,str2)
  result <- httr::GET(Url)
  if(result$status_code == 200){
    #get content from return
    cont = httr::content(result, as="text")
    if(str_detect(cont,"ERROR 50")==TRUE){
      return_df  = data.frame(c(NA,NA,NA))
    } else{
      counts = cont %>%
        textConnection() %>% 
        read.table(., sep=";", header=TRUE) %>%
        formatC(., format = "d", big.mark = ",")
      #data.frame to return
      return_df = data.frame(counts)
    }
    rownames(return_df) = str_replace_all(rownames(return_df), "\\."," ")
    return(return_df)
  }
}

SEMRush_API_domain_overview <- function(domain){
  str1 = "https://api.semrush.com/?key=9ef54d3c4f26358e21754e419e1574ba&type=domain_ranks" #SEMRush API link
  str2 = "&export_columns=Rk,Or,Ot&domain="
  str3 = "&database=us"
  Url = paste0(str1,str2,domain,str3)
  #SEMRush API variables
  result <- httr::GET(Url)
  if(result$status_code == 200){
    #get content from returns
    cont = content(result)
    if(str_detect(cont,"ERROR 50")==TRUE){
      return_df  = data.frame(c(NA,NA,NA))
    } else{
      counts = cont %>%
        textConnection() %>% 
        read.table(., sep=";", header=TRUE) %>%
        formatC(., format = "d", big.mark = ",")
      #data.frame to return
      return_df = data.frame(counts, stringsAsFactors = FALSE)
    }
    rownames(return_df) = str_replace_all(rownames(return_df), "\\."," ")
    return(return_df)
  }
}

SEMRush_API_domain_domains <- function(client, competitor, nkw=10){
  str1 = "https://api.semrush.com/?key=9ef54d3c4f26358e21754e419e1574ba&type=domain_domains&"
  str2 = paste0("&display_sort=co_asc&display_limit=",nkw,"&domains=") #SEMRush API link
  str3 = "&database=us&export_columns=Ph"
  searchString = paste0("*|or|",client,"|*|or|",competitor)
  Url = paste0(str1,str2,searchString,str3)
  #SEMRush API variables
  result <- httr::GET(Url)
  cont = content(result)
  if(result$status_code == 200){
    if(str_detect(cont,"ERROR 50")==TRUE){
      return_df  = data.frame("Select..."="No keywords found")
    } else{
      #get content from returns
      cont2 = cont %>% 
        textConnection() %>% 
        read.table(., sep=";", header=TRUE, stringsAsFactors = FALSE) #%>%
        # formatC(., format = "d", big.mark = ",")
      kw_return = cont2[cont2!="",] #remove blanks
      return_df = data.frame("Select..."=kw_return, stringsAsFactors = FALSE)
    }
  }
  return(return_df)
}

SEMRush_API_domain_keywords <- function(domain){
  str1 = "https://api.semrush.com/?key=9ef54d3c4f26358e21754e419e1574ba&type=domain_ranks" #SEMRush API link
  str2 = "&export_columns=Rk,Or,Ot&domain="
  str3 = "&database=us"
  Url = paste0(str1,str2,domain,str3)
  #SEMRush API variables
  result <- httr::GET(Url)
  cont = content(result)
  if(result$status_code == 200){
    #get content from returns
    cont2 = cont %>% 
      textConnection() %>% 
      read.table(., sep=";", header=TRUE, stringsAsFactors = FALSE) %>%
      formatC(., format = "d", big.mark = ",")
    if(str_detect(cont,"ERROR 50")==TRUE){
      return_df  = data.frame(c(NA,NA,NA))
    } else{
      return_df = cont2 %>% as.data.frame
    }
    return(return_df)
  }
}

SEMRush_API_backlinks <- function(domain, target_type=NULL){
  str1 = "https://api.semrush.com/analytics/v1/?key=9ef54d3c4f26358e21754e419e1574ba&type=backlinks_overview&target=" #SEMRush API link
  #SEMRush API variables
  if(target_type != "domain" & target_type!="url"){
    stop("Error: select target type for search ('domain' or 'url')")
  } else{
      if(target_type=="domain"){
        str2 = "&target_type=domain&export_columns=total,domains_num,urls_num"
        url = paste0(str1,domain,str2)
      }
      if(target_type=="url"){
        str2 = "&target_type=url&export_columns=total,domains_num,urls_num"
        #domain2 = paste0("www.",domain)
        domain2 = str_replace(domain,"www.","")
        url = paste0(str1,domain2,str2)
      }
  }
  result <- httr::GET(url)
  if(result$status_code == 200){
    #get content from returns
    #answers
    cont = httr::content(result, as="text")
    if(str_detect(cont,"ERROR 50")==TRUE){
      return_df  = data.frame(out=c(NA,NA,NA))
    } else{
      #data.frame to return
      cont2 = cont  %>% 
        textConnection() %>% 
        read.table(., sep=";", header=TRUE, stringsAsFactors = FALSE) %>%
        formatC(., format = "d", big.mark = ",")
      return_df = data.frame(cont2, stringsAsFactors = FALSE)
    }
    names(return_df) = domain
    return(return_df)
  }
}

#Get search rankings for client and competitor websites and a given keyword or phrase
SEMRush_API_organic_search <- function(keyword, client, competitor){
    
    inputs. = list(keyword, client, competitor)
    if(any(sapply(inputs., is.null))){
      stop("Error: keyword phrase or domains missing.")
    }
  
    kw = str_replace_all(keyword, " ","+") #format keyword string
    cached = paste0("cache/kw/",kw,"/",kw,".txt")
    
    #check if cahced results exist and if they are recent (less than 14 days old)
    if(file.exists(cached) & difftime(Sys.time(), file.info(cached)$mtime, units = "days") < 14){
      
      df = read.table(cached)
      if(!competitor %in% df$Domain){
        compdat = "Not in Top 20"
        compurl = NA
      } else{ 
        compdat = paste(which(df$Domain %in% competitor)[1])
        compurl = paste(df$Url[which(df$Domain %in% competitor)[1]])
        }
      if(!client %in% df$Domain){
        clientdat = "Not in Top 20"
        clienturl = NA
      } else{ 
        clientdat = paste(which(df$Domain %in% client)[1])
        clienturl = paste(df$Url[which(df$Domain %in% client)[1]])
        } 
      #assemble output
      rankings = data.frame(client = c(clientdat,clienturl), 
                            competitor = c(compdat,compurl), 
                            stringsAsFactors = FALSE
      )
      names(rankings) = c(client, competitor)
      rankings
      
    } else{ #if cached results are not found...run API search
      
      #create local directory to store results
      suppressWarnings( dir.create(paste0("cache/kw/",kw)) )
      #create API search string url
      str1 = "https://api.semrush.com/?type=phrase_organic&key=9ef54d3c4f26358e21754e419e1574ba&phrase=" #SEMRush API link
      str2 =  "&export_columns=Dn,Ur,Fl&database=us&display_limit=20" #SEMRush API variables
      Url = paste0(str1,kw,str2)
      # evaluate result
      result <- httr::GET(Url)
      contentReturned = httr::content(result)
      # check status code of return
      if(result$status_code == 200){
      #get content from returns
        if(str_detect(contentReturned,"ERROR 50")==TRUE){
          if(file.exists("temp/urlforpaout.txt")) file.remove("temp/urlforpaout.txt")
          stop("ERROR: no data returned for keyword phrase")
        } else{ #otherwise, read in data from cache
            df = tryCatch(textConnection(contentReturned) %>% read.csv(., sep = ";") %>% as.data.frame)
            write.table(df, paste0("cache/kw/",kw,"/",kw,".txt"))
        }
        if(sum(df$Domain %in% competitorDomain)==0){
          compdat = "Not in Top 20"
          compurl = NA
        } else{ 
          compdat = paste(which(df$Domain %in% competitor))
          compurl = paste(df$Url[which(df$Domain %in% competitor)])
          }
        if(sum(df$Domain %in% client)==0){
          clientdat = "Not in Top 20"
          clienturl = NA
        } else{ 
          clientdat = paste(which(df$Domain %in% client))
          clienturl = paste(df$Url[which(df$Domain %in% client)])
          } 
        #assemble output
        rankings = data.frame(client = c(clientdat,clienturl), 
                              competitor = c(compdat,compurl)
                              )
        names(rankings) = c(client, competitor)
        return(rankings) 
      
      } else{ 
          stop(paste0("SEMRush database returnd the following error code: ", result$status_code))
        }
    }
  
}

SEMRush_API_get_units <- function(api_key){
  str1 = "http://www.semrush.com/users/countapiunits.html?key=" #SEMRush API link
  Url = paste0(str1,api_key.)
  result <- httr::GET(Url)
  if(result$status_code == 200){
    #get content from returns
    #answers
    cont = httr::content(result)
    if(str_detect(cont,"ERROR 50")==TRUE){
      return_df  = data.frame(NA)
    } else{
      cont = read_html(Url) %>%
        html_nodes("p") %>% 
        str_extract_all(., "[[:digit:]]+") %>% 
        unlist %>% 
        as.numeric
      #data.frame to return
      return_df = data.frame(api_points = formatC(cont, format = "d", big.mark = ","))
    }
    #names(return_df) = c("backlinks_total","domains_num","urls_num")
    return(return_df)
  }
}

formatDomain = function(domain){
  
  #check for http:// or https:// prefix and remove it if necessary
  if(stringr::str_detect(domain,"http://")){
    domain. = str_remove(domain, "http://")
    if(stringr::str_detect(domain.,"www.")){
      domain. = str_remove(domain., "www.")
    }
    if(stringr::str_ends(domain.,"/")){
      domain. = str_remove(domain., "/")
    }
  }
  if(stringr::str_detect(domain,"https://")){
    domain. = str_remove(domain, "https://")
    if(stringr::str_detect(domain.,"www.")){
      domain. = str_remove(domain., "www.")
    }
    if(stringr::str_ends(domain.,"/")){
      domain. = str_remove(domain., "/")
    }
  }
  if(!stringr::str_detect(domain,"http")){
    domain. = domain
    if(stringr::str_detect(domain.,"www.")){
      domain. = str_remove(domain., "www.")
    }
    if(stringr::str_ends(domain.,"/")){
      domain. = str_remove(domain., "/")
    }
  }
  
  return(stringr::str_trunc(domain., width=35))
  
}