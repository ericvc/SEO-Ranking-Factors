
## Install packages from GitHub repositories
#devtools::install_github("Leszek-Sieminski/pagespeedParseR")
#devtools::install_github("nik01010/dashboardthemes")

## Load R Packages
library(shinydashboard)
library(shiny)
library(DT)
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(dashboardthemes)
library(pagespeedParseR)

## Load functions in current working directory
source("functions.R")

## Set page variables
#read-in api key JSON file. 
#Should contain a simple list with key names "semrush_api_key" and "google_lighthouse_api_key"
#See "api_keys_example.json" for template
attach(jsonlite::read_json("api_keys.json"))

#Authorize google lighthouse api
auth_pagespeed2(google_lighthouse_api_key)

#Define list of parameters from Google Lighthouse
parameters = c(
  "score.performance",
  "score.accessibility",
  "performance_first_contentful_paint_displayValue",
  "performance_interactive_displayValue",
  "performance_first_cpu_idle_displayValue",
  "performance_estimated_input_latency_displayValue",
  "performance_uses_responsive_images_score"
)

#Description of parameters from Google Lighthouse
parameters_desc = data.frame(
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
for (i in 1:length(parameters_desc)) {
  parameters_tags[[i]] = tags$a(href = parameters_link[i], parameters_desc[i])
}

#rownames for output dataframe (including in server logic disables the app for some reason)
rn = c(
  "Backlinks to Site URL (total)",
  "Unique Domain Backlinks to Page",
  "Unique URL Backlinks to Page",
  "Keywords in Page Title",
  "Keywords in Page Content",
  "Keywords in Page URL",
  "Content Length (# words)",
  "Homepage"
)

## Create sidebar for app
sidebar <- dashboardSidebar(
  width = 300,
  # App title  w/ company logo in top right corner----
  titlePanel(div(img(
    src = "index.jpg", height = 85
  ), style = "")),
  
  # Text box input to define Domains for comparison
  h4(HTML('<p style="margin-left:5px">1) Enter Domains to Compare</p>'), .noWS = "outside"), 
  textInput("client", "", value = NULL, placeholder = "example.com"),
  # Text box input to define Domains for comparison
  textInput("competitor", "", value = NULL, placeholder = "example.com"),
  h4(HTML('<p style="margin-left:5px">2) How Many Keywords?</p>'), .noWS = "outside"), 
  sliderInput(
    inputId = "num_kw",
    label = "",
    step = 1,
    min = 1,
    max = 25,
    value = 5,
    ticks = FALSE,
    width = "95%"
  ),
  #textInput(inputId = "api_key", label = "Enter SEMRush API Key", placeholder = '9ef54d3c4f26358e21754e419e1574ba'),
  actionButton("search", "Search Keywords", width = "90%"),
  h4(HTML('<p style="margin-left:5px">3) Select Keywords</p>'), .noWS = "outside"), 
  selectInput(
    inputId = "keyword",
    label = "",
    selectize = TRUE,
    choices = c("Search for keywords...")
  ),
  h4(HTML('<p style="margin-left:5px">4) Compare</p>'), .noWS = "outside"), 
  actionButton("submit", "Submit", width = "90%")
)

## Create body of dashboard page
body <- dashboardBody(
  ### changing theme
  shinyDashboardThemes(theme = "grey_light"),
  fluidRow(
    tabBox(
      title = NULL,
      width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      #height = "750px",
      tabPanel(
        "Ranking Factors",
        "",
        h4(textOutput("KeywordTitle")),
        h6(tableOutput("viewKeyword")),
        br(),
        h4(textOutput("DomainTitle")),
        h5(helpText("Information about each site domain.")),
        h6(tableOutput("viewDomains")),
        br(),
        h4(textOutput("PageAttrTitle")),
        h5(
          helpText(
            "If a site ranks in the top 20 for a keyword phrase, information about it will be displayed here."
          )
        ),
        h6(tableOutput("viewPageAttr")),
        br()
      ),
      tabPanel(
        "Page Speed Metrics",
        "",
        h4(textOutput("PageSpeedTitle")),
        h6(tableOutput("viewPageSpeed")),
        h6(helpText(uiOutput(
          "PageSpeedDesc1", inline = TRUE
        ))),
        h6(helpText(uiOutput(
          "PageSpeedDesc2", inline = TRUE
        ))),
        h6(helpText(uiOutput(
          "PageSpeedDesc3", inline = TRUE
        ))),
        h6(helpText(uiOutput(
          "PageSpeedDesc4", inline = TRUE
        ))),
        h6(helpText(uiOutput(
          "PageSpeedDesc5", inline = TRUE
        )))
      ),
      tabPanel("Header Tags", "",
               h4(textOutput(
                 "PageHeaderTagsTitle"
               )),
               h6(tableOutput("viewHeaderTags"))
      ),
      tabPanel(
        title="Google SERP", "",
        textInput("query","Enter Keywords"),
        actionButton("search2","Search Google"),
        br(),
        br(),
        dataTableOutput("SERP", width = "85%"),
        br(),
        br(),
        downloadButton("dl_google_top10 ", "Export to CSV"),
        h6(textOutput("serp_warnings"))
      ),
      tabPanel("View SEMRush API Units", "", 
               h4(textOutput( 
                 "apiUnitsTitle" 
               )), 
               h5(uiOutput("viewAPIUnits")) 
      ),
  fluidRow(infoBoxOutput("tabset1Selected"))
)))

## Create the app
shinyApp(
  
  #UI logic
  ui = dashboardPage(
    dashboardHeader(title = "SEO Tools", titleWidth=300), #theme=shinytheme("cosmo")),
    sidebar,
    body
  ),
  
  #Server logic
  server = function(input, output, session) {
    
    timelapse = 1000*60*60*24*5 #checks API unit total every 5 days (in milliseconds) 
    api_out <- reactivePoll(timelapse, session, 
                            # This function returns the time that log_file was last modified 
                            checkFunc = function() { 
                              Sys.time() 
                            }, 
                            # This function returns the content of log_file 
                            valueFunc = function() { 
                              api_units <- SEMRush_API_get_units(semrush_api_key = semrush_api_key) 
                              timestamp. <- paste0("(",Sys.time(),")") 
                              api_out <- paste(api_units, "units remaining", timestamp.) 
                              return(api_out) 
                            } 
    )
  
    #Get ranking keywords for comparisons
    observeEvent(input$search, {
      progress <- Progress$new(session, min = 1, max = 1)
      on.exit(progress$close())
      progress$set(message = 'Searching for ranking competitive keywords...',
                   detail = '')
      progress$set(value = 1)
      
      client. = formatDomain(input$client)
      competitor. = formatDomain(input$competitor)
      
      kwlist = SEMRush_API_domain_domains(
        client = client.,
        competitor = competitor.,
        nkw = input$num_kw,
        semrush_api_key = semrush_api_key
      ) #returns keywords
      kwlist2 = kwlist[order(kwlist$Select...), ]
      # Can also set the label and select items
      updateSelectInput(
        session = session,
        inputId = "keyword",
        label = "",
        selected = "-Select Keywords-",
        choices = kwlist2
      )
    })
    
    df_out = eventReactive(
      input$submit,
      {
        #Start progress bar and increment
        progress <- Progress$new(session, min = 1, max = 1)
        on.exit(progress$close())
        progress$set(message = 'Gathering SERP rank information...',
                     detail = '')
        progress$set(value = 1)
        
        client. = formatDomain(input$client)
        competitor. = formatDomain(input$competitor)
        
        # ### Build the requested domain output dataset ----
        outList = list() #Precursor to object that will be returned
        
        # get rankings for organic search keywords or phrase
        outList[[1]] = tryCatch(
          SEMRush_API_organic_search(
            keyword = input$keyword,
            client = client.,
            competitor = competitor., 
            semrush_api_key = semrush_api_key
          ),
          error = function(e)
            return(matrix(NA, nrow = 2, ncol = 2))
        ) %>%
          as.data.frame %>%
          apply(., 2, as.character)

      #is there a better way to cache this information?
      write.table(outList[[1]][2, ], "temp/urlforpaout.txt", row.names = FALSE)
      
      #Domain overview
      outList[[2]] = sapply(c(client., competitor.), function(x)
        SEMRush_API_domain_overview(x, semrush_api_key = semrush_api_key)) %>%
        do.call("cbind", .)

      #Domain backlinks information
      outList[[3]] = sapply(c(client., competitor.), function(x)
        SEMRush_API_backlinks(x, target_type = "domain", semrush_api_key = semrush_api_key)) %>%
        do.call("cbind", .)
      
      #Combine all domain data
      out = do.call("rbind", outList) %>%
        data.frame(
          .,
          stringsAsFactors = FALSE
          )
      rownames(out) = c(
        "SERP Rank",
        "SERP URL",
        "Domain Rank",
        "Organic Keywords",
        "Organic Traffic",
        "Total Backlinks to Site Domain",
        "Unique Backlink Domains to Site Domain",
        "Unique Backlink URLs to Site Domain"
      )
      out[1, ][is.na(out[1, ])] <- "Not in top 20"
      out[2, ][is.na(out[2, ])] <- "No data returned"
      #evaluate url output to ensure length is reasonable (40 characters)
      out[2, ] = str_trunc(out[2, ], width = 40)
      
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
      out2 = SEMRush_API_keyword_overview(keyword = input$keyword, 
                                          semrush_api_key = semrush_api_key
                                          ) #%>% t %>% as.data.frame %>% apply(.,2,as.character)
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

      if(file.exists("temp/urlforpaout.txt")){
        Urls = read.table("temp/urlforpaout.txt", header=TRUE, stringsAsFactors = FALSE)[,1]
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
            pa_out$backlinks[,2] = SEMRush_API_backlinks(Urls[2], 
                                                         target_type="url", 
                                                         semrush_api_key = semrush_api_key) %>% 
              unlist %>% 
              as.character
            pa_out$backlinks[is.na(pa_out$backlinks)] <- "No data returned"
            pa_out$crawl[,2] = WebCrawler(Urls[2], keyword = input$keyword) %>% 
              unlist %>%
              as.character
          }
          if(whichMissing==2){
            pa_out$backlinks[,1] = SEMRush_API_backlinks(Urls[1], 
                                                         target_type="url", 
                                                         semrush_api_key = semrush_api_key) %>% 
              unlist %>% 
              as.character
            pa_out$backlinks[is.na(pa_out$backlinks)] <- "No data returned"
            pa_out$crawl[,1] = WebCrawler(Urls[1], keyword = input$keyword) %>%
              unlist %>%
              as.character
          }
        }
        if(length(whichMissing)==0){
          pa_out$backlinks =  lapply(Urls, function(x) SEMRush_API_backlinks(x, 
                                                                             target_type="url", 
                                                                             semrush_api_key = semrush_api_key
                                                                             )
                                     ) %>%
            do.call("cbind",.)
          pa_out$backlinks[is.na(pa_out$backlinks)] <- "No data returned"
          pa_out$crawl = sapply(Urls, function(x) WebCrawler(x, keyword = input$keyword)[,1])  %>%
            apply(.,2,as.character)
        }
        pa_df = do.call("rbind",pa_out) %>% 
          data.frame(.)
        rownames(pa_df) = rn
        names(pa_df) = c(client., competitor.)
        return(pa_df)

      }

    })
    
    lh_out = eventReactive(input$submit, { #add functions to build output data.frame.
      
      progress <- Progress$new(session, min=1, max=4)
      on.exit(progress$close())
      progress$set(message = 'Gathering page speed and accessibility metrics from Google Lighthouse',
                   detail = 'This may take several moments. See descriptions for links to more information about page metrics.')
      progress$set(value = 1)
      
      # check "Performance" and "Accessibility" for Google.com & Bing.com for
      # both desktop & mobile and return in a data frame with most important columns
      Urls = if(file.exists("temp/urlforpaout.txt")){
        read.table("temp/urlforpaout.txt", header=TRUE, stringsAsFactors = FALSE)[,1]
      } else Urls = c(NA,NA)
      whichMissing = which(is.na(Urls))
      
      progress$set(value = 2)  
      
      if(length(whichMissing)>0){
        Urls[whichMissing] <- paste0("http://www.",c(input$client, input$competitor)[whichMissing])
      } 
      progress$set(value = 3)  
      d <- download_lighthouse(
        url=Urls,
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
      names(d2) <- c("",sapply(Urls,formatDomain),sapply(Urls,formatDomain))
      return(d2)
      progress$set(value = 4)  
    
    })
    
    htags_out = eventReactive(input$submit, { #add functions to build output data.frame.
      
      progress <- Progress$new(session, min=1, max=4)
      on.exit(progress$close())
      progress$set(message = 'Gathering header tags',
                   detail = '')
      progress$set(value = 1)
      
      # check "Performance" and "Accessibility" for Google.com & Bing.com for
      # both desktop & mobile and return in a data frame with most important columns
      Urls = if(file.exists("temp/urlforpaout.txt")){
        read.table("temp/urlforpaout.txt", header=TRUE, stringsAsFactors = FALSE)[,1]
      } else Urls = c(NA,NA)
      whichMissing = which(is.na(Urls))
      
      progress$set(value = 2)  
      
      if(length(whichMissing)>0){
        Urls[whichMissing] <- paste0("http://www.",c(input$client, input$competitor)[whichMissing])
      }
      progress$set(value = 3)  
      out = lapply(Urls, function(url) get_H_tags(url)) %>%
        do.call("cbind",.) %>%
        data.frame
      names(out) = sapply(Urls, function(url) formatDomain(url))
      rownames(out) = paste0("h",1:6)
      return(out)
      progress$set(value = 4)  
    })
    
    google_serp <- eventReactive(input$search2, {
      
      progress <- Progress$new(session, min=1, max=3)
      on.exit(progress$close())
      progress$set(message = 'Querying Google.com',
                   detail = '')
      progress$set(value = 1)
      
      dt = get_top_N(input$query, num_results=20)
      
      progress$set(message = 'Gathering top search results',
                   detail = '')
      progress$set(value = 2)
      
      dt2 = cbind(as.numeric(rownames(dt)),dt)
      names(dt2) = c("Rank","Title","URL")
      dt2$URL <- paste0("<a href='",dt2$URL,"'>",dt2$URL,"</a>")
      rownames(dt2) = NULL
      write.csv(dt2, "temp/google_top10.csv", row.names = FALSE)
      
      progress$set(message = 'Formatting table',
                   detail = '')
      progress$set(value = 3)
      
      return(dt2)
      
    })
    
    #Outputs Titles
    output$KeywordTitle = renderText({"Keyword Info"})
    output$DomainTitle = renderText({"Domain Info"})
    output$PageAttrTitle = renderText({"Ranking Page Attributes"})
    output$PageSpeedTitle = renderText({"Google Lighthouse Page Metrics"})
    output$PageHeaderTagsTitle = renderText({"Page Header Tags"})
    output$apiUnitsTitle = renderText({"SEMRush API Units Balance"}) 
    output$SERP <- DT::renderDataTable({google_serp()}, rownames=FALSE, escape=FALSE)
    #Outputs Render
    output$viewDomains = renderTable({df_out()}, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s",width = 850)
    output$viewKeyword = renderTable({kw_out()}, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 500)
    output$viewPageAttr = renderTable({pa_out()}, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 850)
    output$viewPageSpeed = renderTable({lh_out()}, rownames = FALSE, bordered = TRUE, align = "c", spacing = "s", width = 850)
    output$viewHeaderTags = renderTable({htags_out()}, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 850)
    output$serp_warnings = renderText("WARNING: Too many searches in a short time will cause Google to temporarily flag this IP address.")
    output$PageSpeedDesc1 = renderUI(parameters_tags[[1]])
    output$PageSpeedDesc2 = renderUI(parameters_tags[[2]])
    output$PageSpeedDesc3 = renderUI(parameters_tags[[3]])
    output$PageSpeedDesc4 = renderUI(parameters_tags[[4]])
    output$PageSpeedDesc5 = renderUI(parameters_tags[[5]])
    output$viewAPIUnits = renderUI({api_out()})
    
  }
)
