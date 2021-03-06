SEMRush_API_keyword_overview <- function(keyword, semrush_api_key=NULL) {
  kw = str_replace_all(keyword, " ", "+") #format keyword string
  str1 = "https://api.semrush.com/?type=phrase_this&key="
  str2 = "&phrase=" #SEMRush API link
  str3 = "&export_columns=Nq,Nr,Cp&database=us"
  Url = paste0(str1,semrush_api_key, str2, kw, str3)
  result <- httr::GET(Url)
  if (result$status_code == 200) {
    #get content from return
    cont = httr::content(result, as = "text")
    if (str_detect(cont, "ERROR 50") == TRUE) {
      return_df  = data.frame(c(NA, NA, NA))
    } else{
      counts = cont %>%
        textConnection() %>%
        read.table(., sep = ";", header = TRUE) %>%
        formatC(., format = "d", big.mark = ",")
      #data.frame to return
      return_df = data.frame(counts)
    }
    rownames(return_df) = str_replace_all(rownames(return_df), "\\.", " ")
    return(return_df)
  }
}

SEMRush_API_domain_overview <- function(domain, semrush_api_key=NULL) {
  str1 = "https://api.semrush.com/?key="
  str2 = "&type=domain_ranks" #SEMRush API link
  str3 = "&export_columns=Rk,Or,Ot&domain="
  str4 = "&database=us"
  Url = paste0(str1,semrush_api_key, str2, str3, domain, str4)
  #SEMRush API variables
  result <- httr::GET(Url)
  if (result$status_code == 200) {
    #get content from returns
    cont = content(result)
    if (str_detect(cont, "ERROR 50") == TRUE) {
      return_df  = data.frame(c(NA, NA, NA))
    } else{
      counts = cont %>%
        textConnection() %>%
        read.table(., sep = ";", header = TRUE) %>%
        formatC(., format = "d", big.mark = ",")
      #data.frame to return
      return_df = data.frame(counts, stringsAsFactors = FALSE)
    }
    rownames(return_df) = str_replace_all(rownames(return_df), "\\.", " ")
    return(return_df)
  }
}

SEMRush_API_domain_domains <- function(client, competitor, nkw = 10, semrush_api_key=NULL) {
  str1 = "https://api.semrush.com/?key="
  str2 = "&type=domain_domains&"
  str3 = paste0("&display_sort=co_asc&display_limit=", nkw, "&domains=") #SEMRush API link
  str4 = "&database=us&export_columns=Ph"
  searchString = paste0("*|or|", client, "|*|or|", competitor)
  Url = paste0(str1, semrush_api_key, str2, str3, searchString, str4)
  #SEMRush API variables
  result <- httr::GET(Url)
  cont = content(result)
  if (result$status_code == 200) {
    if (str_detect(cont, "ERROR 50") == TRUE) {
      return_df  = data.frame("Select..." = "No keywords found")
    } else{
      #get content from returns
      cont2 = cont %>%
        textConnection() %>%
        read.table(.,
                   sep = ";",
                   header = TRUE,
                   stringsAsFactors = FALSE) #%>%
      # formatC(., format = "d", big.mark = ",")
      kw_return = cont2[cont2 != "", ] #remove blanks
      return_df = data.frame("Select..." = kw_return, stringsAsFactors = FALSE)
    }
  }
  return(return_df)
}

SEMRush_API_domain_keywords <- function(domain, semrush_api_key=NULL){
  str1 = "https://api.semrush.com/?key="
  str2 = "&type=domain_ranks" #SEMRush API link
  str3 = "&export_columns=Rk,Or,Ot&domain="
  str4 = "&database=us"
  Url = paste0(str1,semrush_api_key,str2,str3,domain,str4)
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

SEMRush_API_backlinks <- function(domain, target_type=NULL, semrush_api_key=NULL){
  str1 = "https://api.semrush.com/analytics/v1/?key="
  str2 = "&type=backlinks_overview&target=" #SEMRush API link
  #SEMRush API variables
  if(target_type != "domain" & target_type!="url"){
    stop("Error: select target type for search ('domain' or 'url')")
  } else{
    if(target_type=="domain"){
      str3 = "&target_type=domain&export_columns=total,domains_num,urls_num"
      url = paste0(str1,semrush_api_key,str2,domain,str3)
    }
    if(target_type=="url"){
      str3 = "&target_type=url&export_columns=total,domains_num,urls_num"
      #domain2 = paste0("www.",domain)
      domain2 = str_replace(domain,"www.","")
      url = paste0(str1,semrush_api_key,str2,domain2,str3)
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
SEMRush_API_organic_search <- function(keyword, client, competitor, semrush_api_key=NULL){
  
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
    str1 = "https://api.semrush.com/?type=phrase_organic&key="
    str2 = "&phrase=" #SEMRush API link
    str3 =  "&export_columns=Dn,Ur,Fl&database=us&display_limit=20" #SEMRush API variables
    Url = paste0(str1,semrush_api_key,str2,kw,str3)
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
        compdat = paste(which(df$Domain %in% competitor)[1])
        compurl = paste(df$Url[which(df$Domain %in% competitor)[1]])
      }
      if(sum(df$Domain %in% client)==0){
        clientdat = "Not in Top 20"
        clienturl = NA
      } else{ 
        clientdat = paste(which(df$Domain %in% client)[1])
        clienturl = paste(df$Url[which(df$Domain %in% client)[1]])
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

SEMRush_API_get_units <- function(semrush_api_key){
  str1 = "http://www.semrush.com/users/countapiunits.html?key=" #SEMRush API link
  Url = paste0(str1, semrush_api_key)
  response <- httr::GET(Url)
  if (response$status_code == 200) {
    cont = httr::content(response) %>% 
      html_text()
    if (str_detect(cont, "ERROR 50") == TRUE) {
      return(NA)
    } else{
      api_units = read_html(response) %>% #read result of response
        html_text %>% #grab text from html code
        as.numeric %>% #convert to numeric
        formatC(format = "d", big.mark = ",") #format value (insert commas)
    }
    return(api_units)
  }
}

formatDomain = function(url){
  
  #Remove prefix from url
  str_remove_all(url, pattern = "^(http[s]?://www\\.|http[s]?://|www\\.)") %>%
    str_trunc(width=35) #limit to 35 characters
  
}
