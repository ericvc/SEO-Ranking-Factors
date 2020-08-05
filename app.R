## Install packages from GitHub repositories
#devtools::install_github("Leszek-Sieminski/pagespeedParseR")
#devtools::install_github("nik01010/dashboardthemes")


## Load R Packages
library(shinydashboard)
library(shiny)
library(rmarkdown)
library(DT)
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(dashboardthemes)
library(pagespeedParseR)
library(assertthat)
library(stringr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(listviewer)
library(reactR)
library(shinyjs)


## Load functions in current working directory
src_files <- list.files("R/", full.names = TRUE)
sapply(src_files, function(x) source(x))

## Set up virtual python environment
#Settings are related to contents of .Rprofile
reticulate::virtualenv_create(envname = "python_environment", python = "python3")
reticulate::virtualenv_install(
  envname = "python_environment",
  packages = c("numpy", "bs4", "requests", "pandas"),
  ignore_installed = TRUE
)
reticulate::use_virtualenv("python_environment", required = TRUE)


## Set page variables
#Read-in api key JSON file 'api_keys.json'
#The file should contain a simple list with key names "semrush_api_key" and "google_lighthouse_api_key"
#See the "api_keys_example.json" for a template
attach(jsonlite::read_json("api_keys.json"))


## Authorize google lighthouse api
auth_pagespeed2(google_lighthouse_api_key)


## Define list of parameters from Google Lighthouse
parameters = c(
  "score.performance",
  "performance_first_contentful_paint_displayValue",
  "performance_speed_index_score",
  "performance_largest_contentful_paint_displayValue",
  "performance_interactive_displayValue",
  "performance_total_blocking_time_displayValue",
  "performance_metrics_details_items_cumulativeLayoutShift1"
)


#Description of parameters from Google Lighthouse
parameters_desc = c(
  "",
  "First Contentful Paint marks the time at which the first text or image is painted.\n",
  "Speed Index measures how quickly content is visually displayed during page load.\n",
  "Largest Contentful Paint reports the render time of the largest image or text block visible within the viewport.\n",
  "Time to interactive is the amount of time it takes for the page to become fully interactive.\n",
  "TBT measures the total amount of time that a page is blocked from responding to user input, such as mouse clicks, screen taps, or keyboard presses.\n",
  "CLS measures the sum total of all individual layout shift scores for every unexpected layout shift that occurs during the entire lifespan of the page."
)

parameters_link = c(
  "https://web.dev/performance-scoring/#weightings",
  "https://web.dev/first-contentful-paint",
  "https://web.dev/speed-index/",
  "https://web.dev/lcp/",
  "https://web.dev/interactive/",
  "https://web.dev/lighthouse-total-blocking-time/",
  "https://web.dev/cls/"
)

parameters_tags = c()
for (i in 1:length(parameters_desc)) {
  parameters_tags[i] = paste0("<a href='", parameters_link[i], "' style='color:blue;'>", parameters_desc[i], "</a>")
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

## Create empty JSON file for menu options
write_json("", "temp/traffic.json")


## Get US states name for column menu options
data(state)


## Create sidebar for app
sidebar <- dashboardSidebar(
  width = 300,
  height = "50%",
  # App title  w/ company logo in top right corner----
  titlePanel(div(img(
    src = "index.jpg", height = 85
  ), style = "")),
  
  # Text box input to define Domains for comparison
  h4(HTML('<p style="margin-left:15px">Enter Domains to Compare</p>'), .noWS = "outside"),
  #h5(HTML('<p style="margin-left:5px"><b>-Client Domain</b></p>'), .noWS = "outside"),
  textInput("client", "Client Domain", value = "", placeholder = "example.com"),
  # Text box input to define Domains for comparison
  #h5(HTML('<p style="margin-left:5px"><b>-Competitor Domain</b></p>'), .noWS = "outside"),
  textInput("competitor", "Competitor Domain", value = "", placeholder = "example.com")
)


## Create body of dashboard page
body <- dashboardBody(
  shinyjs::useShinyjs(),
  ### changing theme
  shinyDashboardThemes(theme = "grey_light"),
  fluidRow(tags$head(tags$style(".shiny-output-error{color:blue; font-size: 17px}")),
    tabBox(
      title = NULL,
      width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      #height = "750px"
      tabPanel(
        "Ranking Factors",
        fluidPage(
          column(12, align='center',
        h3(HTML("<b>Domain Comparisons for SEO</b>")),
        h5("Compare the ranking factors of competing domains. Start by selecting the number of competing keywords to retrieve to get a list of the most competitive keywords. Then compare the ranking performance of each domain using that keyword phrase."),
        splitLayout(
          tags$head(tags$style(
            HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
            )),
          cellWidths = c("0%","7%","0%","25%"),
                    numericInput("num_kw", label = "# Keywords", min = 1, max=25, value = 10),
                    tags$style(type="text/css", "#num_kw {text-align:center}"),
                    selectInput(
                      inputId = "keyword",
                      label = "",
                      selectize = TRUE,
                      choices = c("Search for keywords...")
                    )
        ),
        actionButton("submit_rf", "Compare", width = "15%"),
        h4("Keyword Info"),
        h6(tableOutput("viewKeyword")),
        br(),
        h4("Domain Info"),
        h5(helpText("Information about each site domain.")),
        h6(tableOutput("viewDomains")),
        br(),
        h4("Ranking Page Attributes"),
        h5(
          helpText(
            "If a site ranks in the top 20 for a keyword phrase, information about it will be displayed here."
          )
        ),
        h6(tableOutput("viewPageAttr")),
        br()
      ))),
      tabPanel(
        "Page Speed Metrics",
        "",
        fluidPage(
          column(12, align='center',
        h3(HTML("<b>Google Performance Metrics</b>")),
        h5(
          "Lighthouse 6 performance reports summarize how well the web content from competing domains meet established performance thresholds. These metrics are also used by Google's search ranking algorithm for mobile devices."
        ),
        h6(dataTableOutput("viewPageSpeed", width = "85%")),
        actionButton("submit_lh", "Analyze", width = "15%")#,
      ))),
      tabPanel(
        "Page Accessibility Metrics",
        "",
        fluidPage(
          column(12, align='center',
        h3(HTML("<b>Google Accessibility Metrics</b>")),
        h5(
          "Lighthouse 6 accessibility reports summarize how the web content from competing domains complies with standards for accesibility."
        ),
        h5("(Client Domain Only)"),
        h6(dataTableOutput("viewPageAccess", width = "85%")),
        actionButton("submit_lh2", "Analyze", width = "10%"),
        downloadButton("dl_report", label="Download", width="10%")
      ))),
      tabPanel(
        "Header Tags", "",
        fluidPage(
          column(12, align='center',
        h3(HTML("<b>Page Header Tags</b>")),
        h5("Returns the page header tags from competing domains."),
        h6(tableOutput("viewHeaderTags")),
        actionButton("submit_ht", "Analyze", width = "15%")
      ))),
      tabPanel(title = "Google Search",
               fluidPage(
                 height = "100%",
                 column(
                   12,
                   align = "center",
                   h3(HTML("<b>Google Search Engine</b>")),
                   h5(
                     "Use the Google search engine to identify keywords, competitors, and other relevant information."
                   ),
                   splitLayout(
                     cellWidths = c("25%","0%", "8%","0%"),
                     textInput("query", "Enter Keywords"),
                     tags$style(type="text/css", "#query {text-align:center}"),
                     numericInput(
                       "num_results",
                       "Max. Results",
                       min = 1,
                       max = 20,
                       value = 10,
                       step = 1,
                       width = "100%"
                     ),
                     tags$style(type="text/css", "#num_results {text-align:center}")
                   ), 
                   actionButton("search2", "Search Google", icon("refresh")),
                   h5(
                     "Get the top results from the Google search engine for any kewyword phrase. The table below will show the title, URL, and domain of top rated sites."
                   ),
                   br(),
                   br(),
                   dataTableOutput("SERP", width = "85%"),
                   br(),
                   br(),
                   h6(textOutput("serp_warnings"))
                 )
               )),
      tabPanel(
        # addClass(selector = "body", class = "sidebar-collapse"),
        title = "Google Business Reviews",
        fluidPage(
          column(12, align='center',
                 h3(HTML("<b>Google Business Reviews Analytics</b>")),
                 h5(
                   "Query the DataForSEO database to retrieve tabulated reports of the content of Google Business Reviews, including user information, text, and user rating. Report requests may take several minutes to process and complete. All entries are case-sensitive."
                 ),
                 br(),
                 splitLayout(
                   tags$head(tags$style(
                     HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 text-align:center;
                 }
                 ")
                   )),
                   cellWidths = c("0%","20%","0%", "12%","0%", "12%", "7.5%","3%","0%"),
                   textInput(inputId = "dfseo_keyword", "Business Name", placeholder = "TIV Branding"), 
                   tags$style(type="text/css", "#dfseo_keyword {text-align:center}"),
                   textInput(inputId = "dfseo_city", "Location - City", placeholder = "Santa Rosa"),
                   tags$style(type="text/css", "#dfseo_city {text-align:center}"),
                   selectInput(inputId = "dfseo_state", "Location - State", choices = state.name, selected = "California"), 
                   numericInput(
                     "dfseo_depth",
                     label = "Max. Results",
                     min = 10,
                     max = 1000,
                     value = 50,
                     step = 10
                   ),
                   tags$style(type="text/css", "#dfseo_depth {text-align:center}")
                 ),
                 actionButton("dfseo_submit", "Request", width = "10%", icon("bar-chart-o"))
          ),
          br(),
          br(),
          br(),
          br(),
          fluidRow(
            column(12, align="center",
                   plotOutput(
                     "dfseo_plots"#, width = "auto", height = "auto"
                   ),
                   br(),
                   downloadButton("dl_dfseo_plots", "Save .pdf")
            )
          ),
          fluidRow(
            column(12, align="center",
                   dataTableOutput(
                     "dfseo_table", width = "85%", height = "auto"
                   ),
                   br(),
                   downloadButton("dl_dfseo_table", "Save .csv")
              )
          ),
          br()
        )
      ),
      tabPanel("Traffic Analytics", "",
               fluidPage(height="100%",
                         style = "height:100%;margin-left:5%",
                         fluidRow(
                           column(12, align='center',
                                  h3(HTML("<b>Web Traffic Data & Analytics</b>"))
                                  )
                           ),
                         br(),
              splitLayout(cellWidths = c("25%","25%","50%"),
               fluidRow(style="margin-right:0%",
                 column(7, align='center',
                        h5(HTML("<b>1) Enter a target domain and request a data report</b>")),
                        textInput("dfseo_target", "Target Domain", placeholder = "example.com"),
                        tags$style(type="text/css", "#dfseo_target {text-align:center}"),
                        br(),
                        actionButton("dfseo_target_submit", "Request", icon = icon("table")),
                        br(),
                        br()
                 )), 
                 fluidRow(style="margin-left:5%",
                   column(7, align='center',
                          h5(HTML("<b>2) Select a topic from the report</b>")),
                          #h4(HTML("<b>Explore Data Structure</b>")),
                          selectInput(
                            "dfseo_topic",
                            "Select Topic",
                            choices = " "
                          ),
                          br(),
                          downloadButton("dl_traffic", "Save .json"),
                          br(),
                          br()
                   )),
               fluidRow(style="margin-left:2%",
                        column(7, align='left',
                               h5(HTML("<b>3) Explore the data</b>")),
                               reactjsonOutput( "rjed", height = "100%"),
                               textOutput("dfseo_filename")
                        ))
                 )
              )),
      tabPanel("View SEMrush API Units", "",
               fluidPage(column(
                 12,
                 align = 'center',
                 h3(HTML("<b>SEMRush API Units Balance</b>")),
                 h5(
                   "Query the DataForSEO database to retrieve web traffic analytics reports for a target domain. Use the data explorer to access raw traffic data, including monthly volume estimates, traffic sources, and audience behavior. Report requests may take several minutes to process and complete. All entries are case-sensitive."
                 ),
                 h4(uiOutput("viewAPIUnits"))
               )
              )
            )
)))


## Create the app
shinyApp(
  
  ## UI logic
  ui = dashboardPage(
    dashboardHeader(title = "SEO Tools", titleWidth=300), #theme=shinytheme("cosmo")),
    sidebar,
    body
  ),
  
  ## Server logic
  server = function(input, output, session) {
    
    #Reactive functions
    
    #Report remaining SEMRush API units
    timelapse = 1000*10 #checks API unit total every ten minutes for substantial changes
    api_out <- reactivePoll(
      timelapse,
      session,
      # This function returns the time that log_file was last modified
      checkFunc = function() {
        Sys.time()
      },
      # This function returns the content of log_file
      valueFunc = function() {
        api_units <- SEMRush_API_get_units(semrush_api_key)
        api_out <-
          paste(formatC(api_units, big.mark = ",", format = "d"), "units remaining", paste0("(", Sys.time(), ")"))
        #saveRDS(api_units,"temp/api_units.rds")
        return(api_out)
      }
    )
  
    #Get competing keywords for domain comparison
    observeEvent(input$search, {
      
      domains = c(input$client, input$competitor)
      if(any(domains=="")){
        validate(FALSE, message="Error: You must enter domain names to find competing keyword phrases.")
      }

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
    
    
    #Get ranking factors for competing domains
    df_out = eventReactive(
      input$submit_rf,
      {
        
        domains = c(input$client, input$competitor)
        if(any(domains=="")){
          validate(FALSE, message="Error: Missing domain(s).")
        }

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
    
    
    #Keyword metrics
    kw_out = eventReactive(input$submit_rf, {

      domains = c(input$client, input$competitor)
      if(any(domains=="")){
        validate(FALSE, message="Error: Missing domain(s).")
      }


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
    
    
    #Ranking Page Attributes
    pa_out = eventReactive(input$submit_rf, {
      
      domains = c(input$client, input$competitor)
      if(any(domains=="")){
        validate(FALSE, message="Error: Missing domain(s).")
      }


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
            pa_out$crawl[,2] = web_crawler(Urls[2], keyword = input$keyword) %>% 
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
            pa_out$crawl[,1] = web_crawler(Urls[1], keyword = input$keyword) %>%
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
          pa_out$crawl = sapply(Urls, function(x) web_crawler(x, keyword = input$keyword)[,1])  %>%
            apply(.,2,as.character)
        }
        pa_df = do.call("rbind",pa_out) %>% 
          data.frame(.)
        rownames(pa_df) = rn
        names(pa_df) = c(client., competitor.)
        return(pa_df)

      }

    })
    
    
    #Page Speed Metrics
    lh_out = eventReactive(input$submit_lh, { #add functions to build output data.frame.
      
      progress <- Progress$new(session, min=1, max=3)
      on.exit(progress$close())
      progress$set(message = 'Gathering performance reports from Google Lighthouse 6',
                   detail = 'This may take several moments. See descriptions for links to more information about each performance metric.')
      progress$set(value = 1)
      
      urls = c(input$client, input$competitor)
      if(any(urls=="")){
        validate(FALSE, message="Error: Missing domain(s).")
      }
      urls_formatted <- paste0("http://www.", urls)
      
      progress$set(value = 2)  
      
      d <- download_lighthouse(
        url=urls_formatted,
        output_type = "simple", # return the results in a wide data frame
        strategy = c("desktop", # check both desktop and mobile, bind
                     "mobile"),
        interval = 0.5,           # wait 2 seconds between the calls to API
        categories = c("performance", # run performance & accessibility
                       "accessibility"))
      

      d2 = d[d$parameter %in% parameters,] %>%
        as_tibble
      #d2 = data.frame(d2, parameters_tags)
      d2 = rbind(platform = c("device","mobile","mobile","desktop","desktop"),d2)
      d2$parameter <- d2$parameter %>% 
        str_remove(., "performance_") %>% 
        str_remove(., "displayValue") %>%
        str_remove(.,"accessibility_") %>%
        str_remove(.,"score") %>%
        str_replace_all(.,"_"," ") %>%
        str_replace_all(.,"\\."," ") %>% 
        str_to_title(., local="en")
      d2$Description <- c("",parameters_tags)
      d2$parameter <- c("Device","Overall Performance","First Contentful Paint","Speed Index","Largest Contentful Paint",
                        "Time to Interactive","Total Blocking Time","Cumulative Layout Shift")
      names(d2) <- c("",sapply(urls_formatted,formatDomain),sapply(urls_formatted,formatDomain),"Description")
      return(d2)
      
      progress$set(value = 3)  
    
    })
    
    
    #Page Accessibility
    lh_out2 = eventReactive(input$submit_lh2, { #add functions to build output data.frame.
      
      progress <- Progress$new(session, min=1, max=3)
      on.exit(progress$close())
      progress$set(message = 'Gathering accessibility reports from Google Lighthouse 5',
                   detail = 'This may take several moments. See descriptions for links to more information about each accessibility metric.')
      progress$set(value = 1)
      
      urls = c(input$client)
      if(urls==""){
        validate(FALSE, message="Error: Missing domain.")
      }
      urls_formatted <- paste0("http://www.", urls[urls!=""])
      
      progress$set(value = 2)  
      
      d <- download_lighthouse(
        url=urls_formatted,
        output_type = "simple", # return the results in a wide data frame
        strategy = c("desktop", # check both desktop and mobile, bind
                     "mobile"),
        interval = 0.5,           # wait 2 seconds between the calls to API
        categories = "accessibility") %>% 
        data.frame(., stringsAsFactors = FALSE)
      
      vars <- d$parameter
      
      ii <- grepl("_score$|^score", vars)
      scores <- d[ii,]
      scores$parameter <- gsub(pattern = "accessibility_", "", scores$parameter)
      titles <- gsub("_score$", replacement = "_title", x = d$parameter[ii])
      descriptions <- gsub("_score$", replacement = "_description", x = d$parameter[ii])
      d2 <- d[match(titles, d$parameter),]
      d3 <- d[match(descriptions, d$parameter),]
      
      d_final <-
        data.frame(
          d2[,2],
          d3[,2],
          scores,
          stringsAsFactors = FALSE
        )
      
      #Format titles
      names(d_final) <- c("Title","Description","Metric","Mobile","Desktop")
      d_final[d_final=="1"] <- "Yes"; d_final[d_final=="0"] <- "No";d_final[is.na(d_final)] <- "N/A"
      d_final[1,1:2] <- c("Overall Accessibility Score", "Final score based on outcomes of underlying metrics")
      d_final[1,4:5] <- paste0(as.numeric(d_final[1,4:5]) * 100,"/100")
      d_final$Title = d_final$Title %>%
        gsub("`","",.) %>%
        gsub("<","[",.) %>%
        gsub(">","]",.)
      d_final$Description = d_final$Description %>%
        gsub("`","",.) %>%
        gsub("<","[",.) %>%
        gsub(">","]",.) %>%
        gsub("\\[.*\\]\\(.*\\).","",.)
      rownames(d_final) <- 1:nrow(d_final)
      
      progress$set(value = 3, "Exporting report.")
      
      fname = "temp/accessibility_report.csv"
      if(file.exists(fname)){
        file.remove(fname)
      }
      write.csv(d_final, fname, row.names = FALSE)
      
      return(d_final)
      
    })
    
    
    #Header Tags
    htags_out = eventReactive(input$submit_ht, { #add functions to build output data.frame.
      
      progress <- Progress$new(session, min=1, max=4)
      on.exit(progress$close())
      progress$set(message = 'Gathering header tags',
                   detail = '')
      progress$set(value = 1)
      
      urls = c(input$client, input$competitor)
      if(any(urls=="")){
        validate(FALSE, message="Error: Missing domain(s).")
      }

      urls_formatted <- paste0("http://www.", urls)

      progress$set(value = 3)  
      out = lapply(urls_formatted, function(url) get_header_tags(url)) %>%
        do.call("cbind",.) %>%
        data.frame
      names(out) = sapply(urls_formatted, function(url) formatDomain(url))
      rownames(out) = paste0("h",1:6)
      return(out)
      progress$set(value = 4)
      
    })
    
    #Google Business Reviews Function
    google_reviews_fun <- function(){
      
      if(file.exists("temp/greviews/greviews_data.rds")){
        file.remove("temp/greviews/greviews_data.rds")
      }
      
      progress <- Progress$new(session, min=1, max=3)
      on.exit(progress$close())
      progress$set(message = '1) POST Request',
                   detail = 'Sending customized task request to API server.')
      progress$set(value = 1)
      
      progress$set(message = '2) GET Request',
                   detail = 'Waiting for response to task request. This may take several minutes.')
      progress$set(value = 2)
      
      report <-
        dataforseo_google_reviews(
          keyword = input$dfseo_keyword,
          city = input$dfseo_city,
          state = input$dfseo_state,
          depth = input$dfseo_depth,
          usn = dataforseo$usn,
          passw = dataforseo$passw
        )
      
      progress$set(message = '3) Data Received',
                   detail = 'Preparing visulizations.')
      progress$set(value = 3)
      
      
      p1 <- ggplotify::as.grob(report$hist_plot)
      p2 <- ggplotify::as.grob(report$trend_plot)
      d <- report$data
      
      #draw all plots
      grobs <-
        gridExtra::arrangeGrob(
          grobs = list(
            p1,
            p2
          ),
          ncol = 2
        )
      
      pdf("temp/greviews/greviews_plot1.pdf")
        grid::grid.newpage()
        grid::grid.draw(p1)
        grid::grid.text(
          sprintf("Source: Data4SEO - %s", as.Date(Sys.time())),
          x = unit(0.8, "npc"),
          y = unit(0.025, "npc")
        )
      dev.off()
      
      pdf("temp/greviews/greviews_plot2.pdf")
        grid::grid.newpage()
        grid::grid.draw(p2)
        grid::grid.text(
          sprintf("Source: Data4SEO - %s", as.Date(Sys.time())),
          x = unit(0.8, "npc"),
          y = unit(0.025, "npc")
        )
      dev.off()

      saveRDS(report, "temp/greviews/greviews_data.rds")
      
      grid::grid.newpage()
      grid::grid.draw(grobs)

    }
    
    
    #Google Reviews Plotting Reactive
    google_reviews_plots = eventReactive(input$dfseo_submit, google_reviews_fun())
    
    
    #Google Reviews Table Reactive
    google_reviews_table = eventReactive(input$dfseo_submit, {
      tab = readRDS("temp/greviews/greviews_data.rds")
      return(tab$data)
    })
    
    
    #Google SERP Function and Reactive
    google_serp <- function(){

      if(input$query==""){
        validate(FALSE, message="Error: Google search query must not be empty.")
      }

      progress <- Progress$new(session, min=1, max=3)
      on.exit(progress$close())
      progress$set(message = 'Querying Google.com',
                   detail = '')
      progress$set(value = 1)
      
      #get Google results page by calling python code.
      dt = get_top_google_results(input$query, num_results=input$num_results)
      
      progress$set(message = 'Gathering top search results',
                   detail = '')
      progress$set(value = 2)
      
      #format results data.frame for export and display
      dt2 = cbind(as.numeric(rownames(dt)), dt)
      names(dt2) = c("Rank", "Title", "URL")
      url_parse. = xml2::url_parse(dt2$URL)
      dt2$Domain = with(url_parse., paste(scheme, server, sep = "://"))
      dt2$URL <- paste0("<a href='", dt2$URL, "'>", dt2$URL, "</a>")
      rownames(dt2) = NULL

      progress$set(message = 'Formatting table',
                   detail = '')
      progress$set(value = 3)
      
      return(dt2)

    }
    
    google_serp_out <- eventReactive(input$search2, google_serp())
    
    
    #Traffic Analytics
    traffic_data <- eventReactive(input$dfseo_target_submit, {
      
      write_json({}, "temp/traffic.json", overwrite=TRUE)
        
      progress <- Progress$new(session, min=1, max=3)
      on.exit(progress$close())
      progress$set(message = '1) POST Request',
                   detail = 'Sending customized task request to API server.')
      progress$set(value = 1)
      
      progress$set(message = '2) GET Request',
                   detail = 'Waiting for response to task request. This may take several minutes.')
      progress$set(value = 2)
      
      fn <- 
        dataforseo_traffic_analytics(
            target = input$dfseo_target,
            usn = dataforseo$usn,
            passw = dataforseo$passw
          )
      
      progress$set(message = '3) Report Received',
                   detail = 'Preparing data for export.')
      progress$set(value = 3)
      
      file.copy(fn, "temp/traffic.json", overwrite = TRUE)
      return(fn)
      
    })
    
    
    ## UI Outputs
    #Google SERP
    output$SERP <- DT::renderDataTable({google_serp_out()}, rownames=FALSE, escape=FALSE, options = list(
      lengthMenu = list(c(10, -1), c('10', 'All')),
      pageLength = -1
    ))
    
    #Domain Information
    output$viewDomains = renderTable({
      df_out()
    }, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 850)
    
    #Keyword Information
    output$viewKeyword = renderTable({
      kw_out()
    }, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 500)
    
    #Ranking Page Attributes
    output$viewPageAttr = renderTable({
      pa_out()
    }, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 850)
    
    #Page Speed
    output$viewPageSpeed <- DT::renderDataTable({lh_out()}, rownames = FALSE, escape = FALSE, options = list(
      lengthMenu = list(c(10, -1), c('10', 'All')),
      pageLength = -1
    ))
    
    #Page Accessibility
    output$viewPageAccess <- DT::renderDataTable({lh_out2()}, rownames = TRUE, escape = FALSE, options = list(
      lengthMenu = list(c(30, -1), c('30', 'All')),
      pageLength = -1
    ))
    
    #Google Business Reviews - plots
    output$dfseo_plots <-
      renderPlot(google_reviews_plots(), width = 850)
    
    #Google Business Reviews - table
    output$dfseo_table <- DT::renderDataTable({
          google_reviews_table()
        }, rownames = FALSE, escape = FALSE, options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE,
          lengthMenu = list(c(10, 20, -1), c('10','20', 'All')),
          pageLength = 10
        ))
    
    output$dfseo_filename = renderUI(HTML(sprintf("<p style='font-size:0px'>%s</p>",traffic_data())))
    
    #Web Traffic Data & Analytics
    #Display report topic menu options based on report type
    observeEvent(input$dfseo_target_submit, {
      choices <- c(
        "audience",
        "traffic-sources-organic",
        "traffic-sources-ad",
        "traffic-sources-referring",
        "traffic-sources-social",
        "traffic-monthly",
        "sites-similar"
      )
      updateSelectInput(session, "dfseo_topic", choices = choices, selected = "audience")
    })
    
    output$rjed <- renderReactjson({
      fn <- "temp/traffic.json"
      json_string <- read_json(fn)
      out <- list()
      if (input$dfseo_topic == "audience") {
        out = json_string$tasks[[1]]$result[[1]]$traffic$countries[[1]]
      }
      if (input$dfseo_topic == "traffic-countries") {
        out = json_string["tasks"][[1]]["result"][[1]][[1]]$traffic$countries[[1]]
      }
      if (input$dfseo_topic == "traffic-sources-organic") {
        out = json_string$tasks[[1]]$result[[1]]$traffic$sources$search_organic$top_keywords
      }
      if (input$dfseo_topic == "traffic-sources-ad") {
        out = json_string$tasks[[1]]$result[[1]]$traffic$sources$search_ad$top_keywords
      }
      if (input$dfseo_topic == "traffic-sources-referring") {
        out = json_string$tasks[[1]]$result[[1]]$traffic$sources$referring$top_referring
      }
      if (input$dfseo_topic == "traffic-sources-social") {
        out = json_string$tasks[[1]]$result[[1]]$traffic$sources$social$top_socials
      }
      if (input$dfseo_topic == "traffic-monthly") {
        out = json_string$tasks[[1]]$result[[1]]$traffic$estimated
      }
      if (input$dfseo_topic == "sites-similar") {
        out = json_string$tasks[[1]]$result[[1]]$sites$similar_sites
      }

      reactjson( as.list(out), collapsed = FALSE, iconStyle="square", theme="pretty")

    })

    observeEvent(input$rjed_edit, {
      str(input$rjed_edit, max.level=4)
    })
    
    #Download accessibility report
    output$dl_report <- downloadHandler(
      filename = "temp/report.html",
      content = function(file) {
        rmarkdown::render("template.Rmd", output_file = file,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    #Download google reviews data
    output$dl_dfseo_table <- downloadHandler(
      filename = paste0(input$keyword,"_google_reviews.csv"),
      content = function(file) {
        tab <- readRDS("temp/greviews/greviews_data.rds")
        write.csv(tab$data, file)
      }
    )
    
    #Download google reviews plots
    output$dl_dfseo_plots <- downloadHandler(
      filename = 'google_reviews_fig.zip',
      content = function(file) {
        #location of temp file for website screenshot
        fs = list.files("temp/greviews/", pattern = ".pdf", full.names = TRUE)
        zip(zipfile = file, files = fs)
        if (file.exists(paste0(file, ".zip"))) {
          file.rename(paste0(file, ".zip"), file)
        }
      },
      contentType = "application/zip"
    )
    
    #Download traffic analytics report
    output$dl_traffic <- downloadHandler(
      filename = paste0(input$dfseo_target,"_traffic_analytics.csv"),
      content = function(file) {
        json <- read_json("temp/traffic.json")
        write_json(jason, file)
      }
    )
    
    #Output page header tags
    output$viewHeaderTags = renderTable({
      htags_out()
    }, rownames = TRUE, bordered = TRUE, align = "c", spacing = "s", width = 850)
    
    #Ouput API account balance
    output$viewAPIUnits = renderUI({
      api_out()
    })

  }
)
