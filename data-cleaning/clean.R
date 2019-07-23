library(magrittr)
fingerprint <- function(s) tolower(trimws(s))

s <- c("ahmet","Ahmet","ahmet","Ahmet","Ahmet ","ahmET","Ahmet")
s
fingerprint(s)
df <- data.frame(s = s, fingerprint = fingerprint(s), stringsAsFactors = F)

df <- 
  df %>% 
  dplyr::group_by(s) %>%
  dplyr::mutate(c0 = dplyr::n()) %>%
  dplyr::group_by(fingerprint, s) %>%
  dplyr::mutate(c = dplyr::n()) %>%
  dplyr::group_by(fingerprint) %>% 
  dplyr::mutate(u = dplyr::last(s, order_by = c, )) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(change = u == s) 


paste(df$s[!df$change], df$u[!df$change], sep = ">", collapse = " ")
