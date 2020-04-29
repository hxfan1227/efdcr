library(stringr)
library(tidyverse)
library(purrr)
library(data.table)

get_wq_params <- function(card, inp){
  loc <- str_which(inp, card)
  inp[loc[length(loc)]] %>% 
    str_split(' ', simplify = T) %>%
    str_remove_all(card) %>%
    .[str_detect(., '[^|]+')]
}

get_wq_params_value <- function(param, inp){
  loc <- str_which(inp, param)
  value_line <- loc[length(loc)] + 1
  loc_to_update <- get_wq_params(wq_params[PARAM == param, CARD]) %>%
    str_split(' ', simplify = T) %>%
    str_which(param)
  values <- inp[value_line] %>% 
    str_split(' ', simplify = T) %>%
    .[str_detect(., '\\S+')] %>%
    str_remove_all(wq_params[PARAM == param, CARD]) %>%
    .[str_detect(., '[^|]++')] %>% 
    str_split(' ', simplify = T)
  values[loc_to_update]
}

update_wq_params_value <- function(param, new_val, inp){
  loc <- str_which(inp, param)
  value_line <- loc[length(loc)] + 1
  loc_to_update <- get_wq_params(wq_params[PARAM == param, CARD]) %>%
    str_split(' ', simplify = T) %>%
    str_which(param)
  values <- inp[value_line] %>% 
    str_split(' ', simplify = T) %>%
    .[str_detect(., '\\S+')] %>%
    str_remove_all(wq_params[PARAM == param, CARD]) %>%
    .[str_detect(., '[^|]++')] %>% 
    str_split(' ', simplify = T)
  values[loc_to_update] <- new_val
  inp[value_line] <- paste(values, collapse = ' ')
}

write_wq3dwc_inp <- function(inp, path){
  writeLines(inp, file.path(path, 'wq3dwc.inp'))
}

update_wq3dwc <- function(){
  cards <- paste0('C', c('08', '09', 10:29, 45:47))
  
}

inp <- readLines('C:/Users/hxfan/Desktop/wq3dwc.inp')
writeLines(inp, 'C:/Users/hxfan/Desktop/wq3dwc2.inp')
cards <- paste0('C', c('08', '09', 10:29, 45:47))
wq_params_list <- map(cards, get_wq_params)
wq_params <- map_df(1:length(cards), ~ data.table(PARAM = wq_params_list[[.]], CARD = cards[.]))
params_to_update <- sample(wq_params$PARAM, size = 5)


