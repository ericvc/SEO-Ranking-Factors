library(shiny)
library(DT)
library(shinythemes)
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
#devtools::install_github("Leszek-Sieminski/pagespeedParseR")
library(pagespeedParseR)
source("functions.R")

# Page variables
defaultDomain <- "example.com" #default domain that appears in text fields
#semrush_api_key = '9ef54d3c4f26358e21754e419e1574ba'
google_lighthouse_api_key <- "AIzaSyCP6fA8FCmjLSkpnPUdROmTPQ_HKn-cIog" #ericvc2@gmail.com
auth_pagespeed(google_lighthouse_api_key)

parameters = c(
  "score.performance",
  "score.accessibility",
  "performance_first_contentful_paint_displayValue",
  "performance_interactive_displayValue",
  "performance_first_cpu_idle_displayValue",
  "performance_estimated_input_latency_displayValue",
  "performance_uses_responsive_images_score"
)

parameters_desc = data.frame(
  # "",
  # "",
  # "",
  "Estimated Input Latency is an estimate of how long your app takes to respond to user input.",
  "First Contentful Paint marks the time at which the first text or image is painted.\n",
  "First CPU Idle marks the first time at which the page's main thread is quiet enough to handle input.",
  "Time to interactive is the amount of time it takes for the page to become fully interactive.\n",
  "Serve images that are appropriately-sized to save cellular data and improve load time."
)

parameters_link = c(
  "https://web.dev/estimated-input-latency",
  "https://web.dev/first-contentful-paint",
  "https://web.dev/first-cpu-idle",
  "https://web.dev/interactive",
  "https://web.dev/uses-responsive-images"
)

parameters_tags = list()
for(i in 1:length(parameters_desc)){
  parameters_tags[[i]] = tags$a(href=parameters_link[i], parameters_desc[i])
}

ui <- fluidPage(title = "tiv - Compare SEO Ranking Factors", theme = shinytheme("cosmo"),
                tags$head(
                   tags$style(".shiny-notification {position: fixed; top: 90% ;left: 40%") #.shiny-output-error{color:#5698C4;size=40px;};
                  ),
                # tags$h2("Add a shiny app background image"),
                # setBackgroundImage(
                #   src = "https://www.fillmurray.com/1920/1080"
                # ),
               
  # App title  w/ company logo in top right corner----
  titlePanel(div(img(src="index.jpg", height=75), style="")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",

    # Sidebar panel for inputs ----
    sidebarPanel(width=3, #style = "width:25%;", #position=fixed;"

      # Text box input to define Domains for comparison
      h4("1) Enter Domain Names to Compare"),
      textInput("client", "", value = NULL, placeholder = defaultDomain),
      # Text box input to define Domains for comparison
      textInput("competitor", "", value = NULL, placeholder = defaultDomain),
      h4("2) How Many Keywords? (max 25)"),
      numericInput(inputId = "num_kw", label="", min = 1, max=25, value = 5, width = "25%"),
      #textInput(inputId = "api_key", label = "Enter SEMRush API Key", placeholder = '9ef54d3c4f26358e21754e419e1574ba'),
      actionButton("search", "Search Keywords", width = "100%"),
      br(),
      h4("3) Select Keyword for Comparisons"),
      selectInput(inputId = "keyword", label="", selectize = TRUE, choices = c("Search for keywords...")),
      actionButton("submit", "Submit", width = "100%")
      # Clear results
      #actionButton("clear", "Clear Output")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      h4(textOutput("KeywordTitle")),
      # h5(helpText("Keyword information")),
      h6(tableOutput("viewKeyword")),
      br(),
      h4(textOutput("DomainTitle")),
      h5(helpText("Information about each site domain.")),
      h6(tableOutput("viewDomains")),
      br(),
      h4(textOutput("PageAttrTitle")),
      h5(helpText("If a site ranks in the top 20 for a keyword phrase, information about it will be displayed here.")),
      h6(tableOutput("viewPageAttr")),
      br(),
      h4(textOutput("PageSpeedTitle")),
      #h5(helpText("Page speed and accessibility metrics.")),
      h6(tableOutput("viewPageSpeed")),
      h6(helpText(uiOutput("PageSpeedDesc", inline = TRUE))),
      br(),
      br()
    )
   )
  )

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {

  observeEvent(input$search, {
    
    progress <- Progress$new(session, min=1, max=1)
    on.exit(progress$close())
    progress$set(message = 'Searching for ranking competitive keywords...',
                 detail = '')
    progress$set(value = 1)

    client. = formatDomain(input$client)
    competitor. = formatDomain(input$competitor)
     
    kwlist = SEMRush_API_domain_domains(client = client., competitor = competitor., nkw=input$num_kw) #returns keywords
    kwlist2 = kwlist[order(kwlist$Select...),]
    # Can also set the label and select items
    updateSelectInput(session = session, 
                      inputId = "keyword",
                      label = "", selected = "-Select Keywords-",
                      choices = kwlist2
    )
  })
  
  df_out = eventReactive(input$submit, { #add functions to build output data.frame.

    progress <- Progress$new(session, min=1, max=1)
    on.exit(progress$close())
    progress$set(message = 'Gathering SERP rank information...',
                 detail = '')
    progress$set(value = 1)
    
    client. = formatDomain(input$client)
    competitor. = formatDomain(input$competitor)
    
    # ### Build the requested domain output dataset ----
    # get rankings for organic search keywords or phrase
    outList = list()
    
    temp = tryCatch(SEMRush_API_organic_search(keyword = input$keyword, 
                                               client = client., 
                                               competitor = competitor.
                                              ),
                    error = function(e) return(matrix(NA,nrow=2, ncol=2))
    )
    
    outList[[1]] = temp %>%
      as.data.frame %>% 
      apply(.,2,as.character)

    write.table(outList[[1]][2,], "temp/urlforpaout.txt", row.names = FALSE)    
    
    outList[[2]] = sapply(c(client., competitor.), function(x) SEMRush_API_domain_overview(x)) %>% 
      do.call("cbind",.)
    
    outList[[3]] = sapply(c(client., competitor.), function(x) SEMRush_API_backlinks(x, target_type="domain")) %>% 
      do.call("cbind",.)
    
    out = do.call("rbind",outList) %>% data.frame(., stringsAsFactors = FALSE)
    rownames(out) = c("SERP Rank",
                      "SERP URL",
                      "Domain Rank","Organic Keywords","Organic Traffic",
                      "Total Backlinks to Site Domain", 
                      "Unique Backlink Domains to Site Domain",
                      "Unique Backlink URLs to Site Domain") 
    out[1,][is.na(out[1,])] <- "Not in top 20"
    out[2,][is.na(out[2,])] <- "No data returned"
    #evaluate url output to ensure length is reasonable (40 characters)
    out[2,] = str_trunc(out[2,], width = 40)
    
    names(out) = c(client., competitor.)
    return(out)
    
  })
  
  kw_out = eventReactive(input$submit, {
    
    progress <- Progress$new(session, min=1, max=1)
    on.exit(progress$close())
    progress$set(message = 'Gathering keyword information...',
                 detail = '')
    progress$set(value = 1)
    

    # ###(3) Build the requested domain output dataset ----
    # get rankings for organic search keywords or phrase
    out2 = SEMRush_API_keyword_overview(keyword = input$keyword) #%>% t %>% as.data.frame %>% apply(.,2,as.character)
    rownames(out2) = c("Search Volume",
                      "Number of Results",
                      "Competition")
    colName = ifelse(str_length(input$keyword<20), input$keyword, str_trunc(input$keyword,20))
    colnames(out2) = colName
    out2[is.na(out2)] <- "No data returned"
    return(out2)
    
  })
  
  pa_out = eventReactive(input$submit, {
    
    progress <- Progress$new(session, min=1, max=1)
    on.exit(progress$close())
    progress$set(message = 'Gathering information about ranking pages...',
                 detail = '')
    progress$set(value = 1)
    
    
    client. = formatDomain(input$client)
    competitor. = formatDomain(input$competitor)
    
    Urls = if(file.exists("temp/urlforpaout.txt")){
      read.table("temp/urlforpaout.txt", header=TRUE, stringsAsFactors = FALSE)[,1]
    } else Urls = c(NA,NA)
    whichMissing = which(is.na(Urls))

    if(length(whichMissing)==2){
      outString = "No page appears in the top 20 results."
      stop(safeError(outString))
    } else{
      
      pa_out = list()
      pa_out$backlinks  = matrix("", nrow=3, ncol=length(Urls))
      pa_out$crawl  = matrix("", nrow=5, ncol=length(Urls))
      
      if(length(whichMissing)==1){
        if(whichMissing==1){
          pa_out$backlinks[,2] = SEMRush_API_backlinks(Urls[2], target_type="url") %>% unlist %>% as.character
          pa_out$backlinks[is.na(pa_out$backlinks)] <- "No data returned"
          pa_out$crawl[,2] = WebCrawler(Urls[2], keyword = input$keyword) %>% unlist %>% as.character
        }
        if(whichMissing==2){
          pa_out$backlinks[,1] = SEMRush_API_backlinks(Urls[1], target_type="url") %>% unlist %>% as.character
          pa_out$backlinks[is.na(pa_out$backlinks)] <- "No data returned"
          pa_out$crawl[,1] = WebCrawler(Urls[1], keyword = input$keyword) %>% unlist %>% as.character
        }
      }
      if(length(whichMissing)==0){
        pa_out$backlinks =  lapply(Urls, function(x) SEMRush_API_backlinks(x, target_type="url")) %>% 
          #lapply(.,as.character) %>%
          do.call("cbind",.)
        pa_out$backlinks[is.na(pa_out$backlinks)] <- "No data returned"
        pa_out$crawl = sapply(Urls, function(x) WebCrawler(x, keyword = input$keyword)[,1])  %>% 
          apply(.,2,as.character)
      }
    
      pa_df = do.call("rbind",pa_out) %>% data.frame
      rownames(pa_df) = c("Backlinks to Site URL (total)",
                          "Unique Domain Backlinks to Page",
                          "Unique URL Backlinks to Page",
                          "Keywords in Page Title",
                          "Keywords in Page Content",
                          "Keywords in Page URL",
                          "Content Length (# words)",
                          "Homepage")
      names(pa_df) = c(client., competitor.)
      return(pa_df)
    
    }
    
  })
  
  lh_out = eventReactive(input$submit, { #add functions to build output data.frame.
    
    progress <- Progress$new(session, min=1, max=4)
    on.exit(progress$close())
    progress$set(message = 'Gathering page speed and accessibility metrics from Google Lighthouse',
                 detail = 'See links below for information about page metrics')
    progress$set(value = 1)
    
    # check "Performance" and "Accessibility" for Google.com & Bing.com for
    # both desktop & mobile and return in a data frame with most important columns
    Urls = if(file.exists("temp/urlforpaout.txt")){
      read.table("temp/urlforpaout.txt", header=TRUE, stringsAsFactors = FALSE)[,1]
    } else Urls = c(NA,NA)
    whichMissing = which(is.na(Urls))
    
    progress$set(value = 2)  
    
    if(length(whichMissing)==2){
      outString = "No page appears in the top 20 results."
      stop(safeError(outString))
    } else{
    
        if(length(whichMissing)==1){
           Urls. = Urls[-whichMissing]
        progress$set(value = 3)  
        d <- download_lighthouse(
          url=Urls.,
          output_type = "simple", # return the results in a wide data frame
          strategy = c("desktop", # check both desktop and mobile, bind
                       "mobile"),
          interval = 0.5,           # wait 2 seconds between the calls to API
          categories = c("performance", # run performance & accessibility
                         "accessibility"))
        
        d2 = d[d$parameter %in% parameters,] %>%
          as_tibble
        
        d2 = rbind(platform = c("device","mobile","desktop"),d2)
        d2$parameter <- d2$parameter %>% 
          str_remove(., "performance_") %>% 
          str_remove(., "displayValue") %>% 
          str_remove(.,"accessibility_") %>%
          str_remove(.,"score") %>%
          str_replace_all(.,"_"," ") %>%
          str_replace_all(.,"\\."," ") %>% 
          str_to_title(., local="en")
        names(d2) <- c("",formatDomain(Urls.),formatDomain(Urls.))
        return(d2)
      }
    
      if(length(whichMissing)==0){
        Urls. = Urls
        progress$set(value = 3)  
        d <- download_lighthouse(
          url=Urls.,
          output_type = "simple", # return the results in a wide data frame
          strategy = c("desktop", # check both desktop and mobile, bind
                       "mobile"),
          interval = 0.5,           # wait 2 seconds between the calls to API
          categories = c("performance", # run performance & accessibility
                         "accessibility"))
        
        d2 = d[d$parameter %in% parameters,] %>%
          as_tibble
        
        d2 = rbind(platform = c("device","mobile","mobile","desktop","desktop"),d2)
        d2$parameter <- d2$parameter %>% 
          str_remove(., "performance_") %>% 
          str_remove(., "displayValue") %>%
          str_remove(.,"accessibility_") %>%
          str_remove(.,"score") %>%
          str_replace_all(.,"_"," ") %>%
          str_replace_all(.,"\\."," ") %>% 
          str_to_title(., local="en")
        names(d2) <- c("",sapply(Urls.,formatDomain),sapply(Urls.,formatDomain))
        return(d2)
      }
    }
    progress$set(value = 4)  
  })
  
  output$KeywordTitle = renderText({"Keyword Info"})
  output$DomainTitle = renderText({"Domain Info"})
  output$PageAttrTitle = renderText({"Ranking Page Attributes"})
  output$PageSpeedTitle = renderText({"Google Lighthouse Page Metrics"})
  # output$PageSpeedDesc = renderTable(parameters_desc %>% t, colnames = FALSE, spacing = "xs", rownames=FALSE, align = "c", width = 850)
  output$PageSpeedDesc = renderUI(parameters_tags)
  output$viewDomains = renderTable({df_out()}, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s",width = 850)
  output$viewKeyword = renderTable({kw_out()}, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 500)
  output$viewPageAttr = renderTable({pa_out()}, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 850)
  output$viewPageSpeed = renderTable({lh_out()}, rownames = FALSE, bordered = TRUE, align = "c", spacing = "s", width = 850)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

