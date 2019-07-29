library(magrittr)
fingerprint <- function(s) tolower(trimws(s))

s <- c("ahmet","Ahmet","ahmet","Ahmet","Ahmet ","ahmET","Ahmet")
s2 <- s
fingerprint(s)
cleaning <- function(s = s2){
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
  
  to_update <- 
    df %>%
    dplyr::filter(will_be_updated) %>%
    dplyr::mutate(
      prio_to_update = (count + count_combination * .01 + l * .001) / nrow(df),
      final_value_to_keep = as.character(NA)
    ) %>%
    dplyr::arrange(dplyr::desc(prio_to_update), dplyr::desc(count), dplyr::desc(count_combination), l) %>%
    #   dplyr::select(s, fingerprint, value_to_keep) %>% 
    dplyr::distinct()
  
  for (i in 1:nrow(to_update)) {
    to_update$final_value_to_keep[i] <- should_replace_string(
      x = to_update$s[i], 
      replacement = to_update$value_to_keep[i])
  }
  
  df2 <- dplyr::left_join(
    df, 
    dplyr::select(to_update, s, final_value_to_keep),
    by = "s")
  
  res <- dplyr::coalesce(df2$final_value_to_keep, df2$s)
  return(res)
}

should_replace_string <- function(x = "NoT CleAn ", replacement = "Clean"){
  input <- readline(prompt = paste0("Wanna replace '", x, "' with '", replacement,  "': y/n or the value desired: "))
  if(tolower(input) == "y")
  {
    res <- replacement
    attributes(res) <- list(to_replace = T)
  } else {
    if(tolower(input) == "n")
    {
      res <- x
      attributes(res) <- list(to_replace = F)
    } else {
      res <- input
      attributes(res) <- list(to_replace = NULL)
    }
  }
  
  return(res)
}
