# devtools::install_git(url = "https://github.com/bhakyuz/sahibinden")
library(sahibinden)
library(dplyr)

# istanbul-eyupsultan for sale, 5516 classified in 111 pages 
initial_url <- "https://www.sahibinden.com/satilik-daire/istanbul-eyupsultan?pagingSize=50"
cat(paste("Getting searches from", initial_url, "\n"))
next_url <- initial_url

while (!is.na(next_url)) {
  s <- sahibinden::search_for_sale(page_url = next_url)
  xml2::write_html(x = s$content, file = paste0("downloaded/", s$hashed_url,".html"))
  next_url  <- s$next_page_url
  cat(paste(s$meta$current_page, "/", s$meta$pages, "\n"))
  
  logs <- tibble(
    hashed_url = s$hashed_url,
    url = s$url,
    timestamp = lubridate::now()
    ) %>% bind_cols(
      as.data.frame(s$meta)
    )
  write.table(logs, file = "logs.csv", na = "", row.names = F, append = T, col.names = F)
  Sys.sleep(30)
}
