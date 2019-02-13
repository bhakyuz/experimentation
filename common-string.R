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
common <- common_expressions$expression
# common <- c("send you", "request for")

keep_only_common <- function(s, common){
  len <- stringi::stri_length(s)
  empty <- stringi::stri_join(rep("#", len), collapse = "")
  matches <- stringi::stri_locate_first_fixed(s, common)

  for(i in 1:nrow(matches)){
    stringi::stri_sub(empty, matches[i, 1], matches[i, 2]) <- 
      stringi::stri_sub(s, matches[i, 1], matches[i, 2]) 
  }
  final <- stringi::stri_replace_all_regex(empty, pattern = "#+", replacement = " # ")
  return(stringi::stri_trim(final))
}

keep_only_common(s, common)
df2 <- dplyr::bind_rows(df,df,df,df,df,df,df,df,df,df,df,df,df,df,df) %>%
  rowwise() %>%
  mutate(sentence_common = keep_only_common(sentence, common[3:4]))


   