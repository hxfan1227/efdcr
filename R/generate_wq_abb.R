generate_wq_abb <- function(x){
  str_extract_all(x[1], ',.*\\(', simplify = T) %>%
    str_sub(2, -2) %>% str_trim() -> fullnames
  fullnames %>% str_extract_all('[A-Z]', simplify = T) -> abbs
  if(length(abbs) >=2){
    return(paste(abbs, collapse = ''))
  }
  return(fullnames)
}


