#' Function for authenticating user token to use Google Lighthouse API
#'
#' @description This function pulls the title and URL of the top-ranked web pages
#' from a given Google search query, and returns the information in the form of a data.frame object.
#' @details #The Google authentication function from the 'pagespeedParseR' package had a broken link.
#' This function is replicated from that package with a modified URL for authentication.
#'
#' @param api_key string. API authentication key from the user's Google Search Console account.
#' @param verbose logical. Print to console if authentication was successful.
#'
#' @import assertthat
#' @import httr
#'
auth_pagespeed2 <- function(api_key, verbose = TRUE){
  require(assertthat)
  require(httr)
  assert_that(noNA(api_key), not_empty(api_key), is.string(api_key),
              nchar(api_key) > 0, noNA(verbose), not_empty(verbose),
              is.logical(verbose))
  x <- GET(url = "https://www.googleapis.com/pagespeedonline/v5/runPagespeed", 
           query = list(url = "https://www.w3.org/", key = api_key, 
                        strategy = "desktop"))
  Sys.sleep(0.5)
  if (x$status_code == 200) {
    Sys.setenv(PAGESPEED_API_KEY = api_key)
    if(verbose) 
      message("API key authorized.")
  }
  else {
    stop(paste0("Authorization error: HTTP status code ", 
                x$status_code, ". Check your API key."))
  }
}
