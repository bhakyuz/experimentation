library(dplyr)
library(tidytext)
t <- c(
  "ahmet send you a request for tatata totoo",
  "mehmet send you a request for today lunch",
  "alban send you a request for dinner",
  "bahadir send you a request for 3 seats",
  "kemal send you a request for Sat"
)

df <- tibble(
  sentence = t,
  sentence2 = t,
  len = stringi::stri_length(t)
  ) 


expressions <- 
  tidytext::unnest_tokens(tbl = df, output = expression, input = sentence2, 
                          token = "ngrams", drop = T, n = 2) %>%
  dplyr::bind_rows(
    tidytext::unnest_tokens(tbl = df, output = expression, input = sentence2, 
                            token = "ngrams", drop = T, n = 3)
  ) %>%
  rowwise() %>%
  mutate(
    s = stringi::stri_locate_first_fixed(str = sentence, pattern = expression)[1],
    e = stringi::stri_locate_first_fixed(str = sentence, pattern = expression)[2]
  ) %>%
  ungroup()

common_expressions <- 
  expressions %>%
  group_by(expression) %>% 
  summarise(n = n_distinct(sentence)) %>%
  arrange(desc(n)) %>%
  filter(n > 2) %>%
  mutate(is_common = 1) %>%
  select(-n)

expressions2 <-
  expressions %>% 
  left_join(common_expressions) %>%
  filter(is_common == 1) %>%
  rowwise() %>%
  mutate(
    vector = list(c(rep(0, s-1), rep(1, e-s+1), rep(0, len-e)))
  )


s <- "ahmet send you a request for tatata totoo"
common <- common_expressions$expression %>% unique()

keep_only_common <- function(s, common){
  len <- stringi::stri_length(s)
  c <- stringi::stri_locate_first_fixed(s, common)
  stri_sub(s, from = c[,1], to = c[,2])
  v <- 1:len
  v <= c[1,1] & v >= c[1,2]
  check <- sapply(1:nrow(c), function(x) as.numeric(v >= c[x,1] & v <= c[x,2]))
  tokeep <- as.numeric(rowSums(check) >= 1)
  s_v <- unlist(strsplit(s, fixed = T, split = ""))
  s_v[!as.logical(tokeep)] <- "#"
  t <- paste0(s_v, collapse = "")
  t2 <- stringi::stri_replace_all_regex(t, pattern = "#+", replacement = " # ")
  return(stringi::stri_trim(t2))
}

df2 <- df %>%
  rowwise() %>%
  mutate(sentence_common = keep_only_common(sentence, common[3:4]))


   