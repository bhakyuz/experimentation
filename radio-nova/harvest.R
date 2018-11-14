library(magrittr)
harvest <- function(){
  

  req_url <- httr::parse_url("http://www.nova.fr/radionova/radio-nova")

  api_response <- httr::POST(url = httr::build_url(req_url),
                             encode = 'json'
                             # httr::verbose()
  )
  api_content <- httr::content(api_response)
  
  # day, month string, year, hour+h, minute 
  selected_ts_text <- api_content %>% rvest::html_nodes(".select") %>% 
    rvest::html_node("[selected]") %>% rvest::html_text()
  
  selected_ts <- lubridate::dmy_hm(paste0(selected_ts_text, collapse = ""), tz = "Europe/Paris")
  selected_date <- lubridate::as_date(selected_ts)
  songs <- api_content %>%
    rvest::html_nodes(".square-item.col-xs-6.col-sm-4.col-md-3") 
  
  # artists
  songs %>% rvest::html_node(".name") %>% rvest::html_text()
  # title
  songs %>% rvest::html_node(".description") %>% rvest::html_text()
  # pictures
  songs %>% rvest::html_node("picture") %>% rvest::html_node("[srcset]") %>%
    rvest::html_attr("srcset")
  
  # timing
  hm <- songs %>% rvest::html_node(".time.icon-time") %>% rvest::html_text() %>% 
    stringi::stri_trim()
  secs <- lubridate::period_to_seconds(lubridate::hm(hm))
  # if the first element has the max, then all dates should be same
  # if not, date should be one day earlier from the mex sec and so on 
  diff_selected_date_to_song <- rep(0, length(secs))
  break_point <- which(secs == max(secs))
  if (break_point > 1) diff_selected_date_to_song[break_point:length(secs)] <- -1
  dates <- rep(selected_date, length(secs)) - diff_selected_date_to_song
  
  played_at <- lubridate::ymd_hm(paste(dates, hm), tz = "Europe/Paris") 
    
  
  
  
  
  
}