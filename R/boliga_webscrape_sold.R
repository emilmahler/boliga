#' This is the primary function to webscrape boliga
#' for the actual sale prices
#'
#' The function takes the search results from the base url
#' and iterates until there's no "next" page.
#'
#' @param min_sale_date Sale date from.
#' @param max_sale_date Sale date to.
#' @param type Type.
#' @param zipcodeFrom Postal code from.
#' @param zipcodeTo Postal code to.
#' @export
#' @importFrom magrittr %>%
boliga_webscrape_sold <- function(min_sale_date, max_sale_date, type, zipcodeFrom, zipcodeTo){
  
  base_url <- boliga_create_base_url(min_sale_date = min_sale_date,
                                     max_sale_date = max_sale_date,
                                     type = type,
                                     zipcodeFrom = zipcodeFrom,
                                     zipcodeTo = zipcodeTo)
  
  boliga_base <- httr::GET(url = base_url)
  boliga_base_content <- httr::content(boliga_base, as = "text", encoding = "UTF-8")
  
  boliga_base_html <- xml2::read_html(boliga_base_content)
  
  page_count <- boliga_base_html %>%
    rvest::html_nodes(".page-button") %>%
    lapply(function(x) rvest::html_text(x)) %>%
    lapply(function(x) as.numeric(x)) %>%
    unlist() %>%
    max(na.rm = T)
  
  # loop over the pages and save the
  # results to the results_list.
  pb <- progress::progress_bar$new(total = page_count)
  result_list <- vector("list", page_count)
  for(page in 1:page_count){
    
    url_address <- glue::glue('{base_url}', '&p={page}')
    
    result_list[[page]] <- boliga_get_table(url_address)
    
    Sys.sleep(time = rgamma(n = 1, shape = 3, scale = 0.3))
    pb$tick()
  }
  
  boliga_table <-
    result_list %>%
    dplyr::bind_rows()
  
  boliga_table
}

