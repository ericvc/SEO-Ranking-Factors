dataforseo_google_reviews <-
  function(keyword,
           city="Santa Rosa",
           state="California",
           depth = 10,
           usn,
           passw) {
    #Load python function for scraping city coordinates from location data
    reticulate::source_python("Python/coordinates.py")
    #Load python function for handling requests to D4SEO database
    reticulate::source_python("Python/client.py")
    
    if(depth %% 10 != 0){
      depth <- round(depth/10)*10
      warning(sprintf("Converting depth to multiple of ten: %s", depth))
    }
    
    #create class object
    crds <- coord_scraper(city, state)
    #run 'get_coords' function to scrape coordinates for this location
    crds$get_coords()
    if (any(is.nan(crds$coords %>% unlist))) {
      stop("This location could not be found. Please try again.")
    }
    
    r <-
      RestClientReviews(
        username = usn,
        password = passw,
        lat = crds$lat,
        lon = crds$lon,
        depth = depth,
        keyword = keyword
      )
    
    #POST: Send data object to API, specifying the task
    post <- r$send_post()
    #save the task ID that is returned in the response
    post_tag <- (post$tasks %>% unlist)["data.tag"]
    post_id <- (post$tasks %>% unlist)["id"]
    post_status <- post$status_code
    post_status_message <- post$status_message
    
    if (post_status != 2e4) {
      stop(
        sprintf(
          "Something went wrong (POST request): %s (Code %s) - More details: https://docs.dataforseo.com/v3/appendix/errors/?php.",
          post_status_message,
          post_status
        )
      )
    }
    
    #GET: Intermittently check the API "tasks" queue for the report to become available
    get_status <- 40602 #"Task in Queue."
    i = 0
    i_max = 15
    #simple 'while' loop to check the status of the report request
    while (get_status != 2e4) {
      get_ <- r$send_tasks(post_id)
      get_status <- (get_$tasks %>% unlist)["status_code"]
      get_status_message <- (get_$tasks %>% unlist)["status_message"]
      get_id <- (get_$tasks %>% unlist)["id"]
      if (i > i_max) {
        stop(
          sprintf(
            "Something went wrong (GET request): %s (Code %s) - More details: https://docs.dataforseo.com/v3/appendix/errors/?php.",
            get_status_message,
            get_status
          )
        )
      }
      i = i + 1
      if (get_status == 40102) {
        stop(
          sprintf(
            "Something went wrong (GET request): %s (Code %s) - More details: https://docs.dataforseo.com/v3/appendix/errors/?php.",
            get_status_message,
            get_status
          )
        )
      }
      if (get_status != 2e4) {
        j = ifelse(i <= 5, 1 / i, 1 / 6)
        Sys.sleep(60 * j) #give the program time to respond to the query
      }
    }
    
    #If status is good, unpack data, visualize results, and return
    if (post_id == get_id & get_status == 2e4) {
      filename = sprintf("%s/temp/get/%s.json", getwd(), post_id)
      x <- jsonlite::read_json(filename, simplifyVector = TRUE)
      tab <- x$tasks$result[[1]]$items[[1]]
      
      #Extract digit from "time ago" description
      time_number <-
        stringr::str_extract(tab$time_ago, "[\\d+|^a]") %>%
        replace(., which(. == "a"), 1) %>%
        as.numeric
      #Extract units from "time ago" description
      time_units <-
        stringr::str_extract(tab$time_ago, "\\w+ ago$") %>%
        gsub("\\sago$", "", .) %>%
        gsub("s$", "", .)
      #Multiplication factor to put all time measurements in units of days (roughly)
      time_fac = with(tab, ifelse(grepl("day", time_ago), 1,
                                  ifelse(
                                    grepl("week", time_ago), 7,
                                    ifelse(grepl("month", time_ago), 30, 365)
                                  )))
      
      #Create new variable with standardized naming
      time_ago <- paste(time_number, time_units)
      #Report keyword
      kw <- x$tasks$result[[1]]$keyword %>%
        stringr::str_to_title()
      #Get variables from complete report
      info <- tab[, c("profile_name", "review_text")]
      ratings <- tab$rating$value
      profile_name = info[, 1]
      review_text = info[, 2]
      #Combine into tibble
      d <- cbind(profile_name, ratings, time_ago, review_text) %>%
        as_tibble()
      
      #Plot variables
      cols <-
        c("#4AB2C277", "#198ACC", "#30A19D", "#5C55AD", "#281D9C")
      
      #Plot 1 - Histogram of Customer Ratings
      avg_rating = mean(as.numeric(ratings), na.rm = TRUE) %>%
        round(., 2)
      subtitle. = sprintf("%s - %s average rating based on %d reviews",
                          kw,
                          avg_rating,
                          nrow(d))
      d_freq = table(factor(d$ratings, 1:5)) %>%
        data.frame()
      plt1 <- ggplot(d_freq, aes(x = Var1, y = Freq)) +
        ggtitle("Google Business Reviews", subtitle = subtitle.) +
        # geom_point() +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(size = 1.3, color = "gray85"),
          axis.text.x = element_text(size = 16.5, face = "italic"),
          axis.text.y = element_text(size = 16.5),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 18.5),
          plot.title = element_text(
            size = 22,
            face = "bold",
            hjust = 0.5
          ),
          plot.subtitle = element_text(
            size = 15.5,
            face = "italic",
            hjust = 0.5
          )
        ) +
        geom_bar(
          color = "gray20",
          size = 0.7,
          fill = cols,
          stat = 'identity'
        ) +
        scale_x_discrete("Rating") +
        scale_y_continuous("Count")
      
      #Plot 2 - Trends of Customer Ratings
      time_xy = 0 - (time_number * time_fac)
      d2 <- d
      d2$ratings = as.numeric(d2$ratings)
      d2$color = cols[d2$ratings]
      d2$time = time_xy = as.Date(Sys.time()) - time_number * time_fac
      plt2 <- ggplot(data = d2, aes(x = time, y = ratings)) +
        ggtitle("Google Business Reviews",
                subtitle = sprintf("%s - Customer Ratings Over Time", kw)) +
        geom_smooth(color = "maroon",
                    size = 1.4,
                    se = FALSE) +
        geom_point(
          size = 5,
          position = position_jitter(0.25, 0.25, 222),
          fill = d2$color,
          color = d2$color,
          shape = 21
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          # panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(size = 1.3, color = "gray85"),
          axis.text.x = element_text(size = 15.5, face = "italic"),
          axis.text.y = element_text(size = 16.5),
          axis.title.y = element_text(size = 18.5),
          axis.title.x = element_text(size = 18.5),
          plot.title = element_text(
            size = 22,
            face = "bold",
            hjust = 0.5
          ),
          plot.subtitle = element_text(
            size = 15.5,
            face = "italic",
            hjust = 0.5
          )
        ) +
        scale_y_continuous("Rating",
                           breaks = 1:5,
                           limits = c(0.7, 5.5)) +
        scale_x_date("")
      
      output = list(
        data = d,
        keyword = kw,
        get = get_,
        post = post,
        get = get_,
        hist_plot = plt1,
        trend_plot = plt2
      )
      
      return(output)
      
    }
    
  }
