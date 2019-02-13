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
common <- c("send you", "request for")

keep_only_common <- function(s, common){
  s2 <- s
  c <- stringi::stri_locate_first_fixed(s, common)
  # c[,3] <-  c[,2] - c[,1] + 1
  
  for(i in 1:nrow(c)){
    stringi::stri_sub(s2, c[i, 1], c[i, 2]) <- paste0(rep("#", c[i, 2]-c[i, 1]+1), collapse = "")
  }
  # TODO with similar for up there, loop again in c
  
  c2 <- stri_locate_all_regex(str = s2, pattern = "(?!#)")[[1]]
  for(i in 1:nrow(c2)){
    stringi::stri_sub(s, c2[i, 1], c2[i, 1]) <- "#"
  }
  final <- stringi::stri_replace_all_regex(s, pattern = "#+", replacement = " # ")
  
  return(stringi::stri_trim(final))
}

keep_only_common(s, common)
df2 <- df %>%
  rowwise() %>%
  mutate(sentence_common = keep_only_common(sentence, common[3:4]))


   