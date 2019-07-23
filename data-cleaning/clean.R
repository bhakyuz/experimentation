library(magrittr)
fingerprint <- function(s) tolower(trimws(s))

s <- c("ahmet","Ahmet","ahmet","Ahmet","Ahmet ","ahmET","Ahmet")
s
fingerprint(s)
df <- data.frame(s = s, fingerprint = fingerprint(s), stringsAsFactors = F)

df <- 
  df %>% 
  dplyr::mutate(l = nchar(s)) %>%
  dplyr::group_by(s) %>%
  dplyr::mutate(count = dplyr::n()) %>%
  dplyr::group_by(fingerprint, s) %>%
  dplyr::mutate(count_combination = dplyr::n()) %>%
  dplyr::group_by(fingerprint) %>% 
  dplyr::mutate(value_to_keep = dplyr::last(s, order_by = count, )) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(will_be_updated = !value_to_keep == s) 

# paste(df$s[!df$change], df$u[!df$change], sep = ">", collapse = " ")

df %>%
  dplyr::filter(will_be_updated) %>%
  dplyr::mutate(prio_to_update = (count + count_combination * .01 + l * .001) / nrow(df) ) %>%
  dplyr::arrange(dplyr::desc(prio_to_update), dplyr::desc(count), dplyr::desc(count_combination), l) %>%
#   dplyr::select(s, fingerprint, value_to_keep) %>% 
  dplyr::distinct()
