dataforseo_traffic_analytics <-
  function(target,
           usn,
           passw) {
    
    #Load python function for handling requests to D4SEO database
    reticulate::source_python("Python/client.py")

    r <- 
      RestClientTrafficAnalytics(
        username=dataforseo$usn,
        password=dataforseo$passw,
        target = target
      )
    
    #POST: Send data object to API, specifying the task
    post <- r$send_post()
    #save the task ID that is returned in the response
    post_tag <- (post$tasks %>% unlist)["data.tag"]
    post_id <- (post$tasks %>% unlist)["id"]
    post_status <- post$status_code
    post_status_message <- post$status_message
    
    if(post_status != 2e4) {
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
    i_max = 25
    #simple 'while' loop to check the status of the report request
    while (get_status != 2e4) {
      get_ <- r$send_tasks(post_id)
      get_status <- (get_$tasks %>% unlist)["status_code"]
      get_status_message <- (get_$tasks %>% unlist)["status_message"]
      get_id <- (get_$tasks %>% unlist)["id"]
      if (i > i_max) {
        stop(
          sprintf(
            "Operation timed out. Please try again later.",
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
    if(post_id == get_id & get_status == 2e4){
      
      filename = sprintf("%s/temp/get/%s.json", getwd(), post_id)
      # file.copy(filename, "temp/traffic.json", overwrite = TRUE)
      return(filename)
      
    }
    
  }

