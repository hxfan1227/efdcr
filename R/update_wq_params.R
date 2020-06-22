#' @import stringr
NULL
#' Update the water quality parameters in \code{wq3dwc.inp}.
#' @param card the CARD which the parameter belongs to.
#' @param inp the content of the \code{wq3dwc.inp} file.
#' @param param the name of the parameter to be updated.
#' @param new_val the new value of the target water quality parameter.
#' @export
update_wq_params_value <- function(param, new_val, inp){
  loc <- stringr::str_which(inp, param)
  value_line <- loc[length(loc)] + 1
  loc_to_update <- get_wq_params(wq_params[PARAM == param, CARD], inp) %>%
    stringr::str_split(' ', simplify = T) %>%
    stringr::str_which(param)
  values <- inp[value_line] %>% 
    stringr::str_split(' ', simplify = T) %>%
    .[stringr::str_detect(., '\\S+')] %>%
    stringr::str_remove_all(wq_params[PARAM == param, CARD]) %>%
    .[stringr::str_detect(., '[^|]++')] %>% 
    stringr::str_split(' ', simplify = T)
  values[loc_to_update] <- new_val
  inp[value_line] <- paste(values, collapse = ' ')
  invisible(inp) 
}

NULL
#' Get the available water quality parameters.
#' @rdname update_wq_params_value
#' @export
get_wq_params <- function(card, inp){
  loc <- stringr::str_which(inp, card)
  inp[loc[length(loc)]] %>% 
    stringr::str_split(' ', simplify = T) %>%
    stringr::str_remove_all(card) %>%
    .[stringr::str_detect(., '[^|]+')] # Using the dot place-holder
}

NULL
#' Get the value of the selected water quality parameter.
#' @rdname update_wq_params_value
#' @export
get_wq_params_value <- function(param, inp){
  loc <-  stringr::str_which(inp, param)
  value_line <- loc[length(loc)] + 1
  loc_to_update <- get_wq_params(wq_params[PARAM == param, CARD], inp) %>%
    stringr::str_split(' ', simplify = T) %>%
    stringr::str_which(param)
  values <- inp[value_line] %>% 
    stringr::str_split(' ', simplify = T) %>%
    .[stringr::str_detect(., '\\S+')] %>%
    str_remove_all(wq_params[PARAM == param, CARD]) %>%
    .[stringr::str_detect(., '[^|]++')] %>% 
    stringr::str_split(' ', simplify = T)
  values[loc_to_update]
}


# write_wq3dwc_inp <- function(inp, path){
#   writeLines(inp, file.path(path, 'wq3dwc.inp'))
# }
# 
# update_wq3dwc <- function(){
#   cards <- paste0('C', c('08', '09', 10:29, 45:47))
#   
# }
# 
# inp <- readLines('C:/Users/hxfan/Desktop/wq3dwc.inp')
# writeLines(inp, 'C:/Users/hxfan/Desktop/wq3dwc2.inp')
# cards <- paste0('C', c('08', '09', 10:29, 45:47))
# wq_params_list <- map(cards, get_wq_params, inp = inp)
# wq_params <- map_df(1:length(cards), ~ data.table(PARAM = wq_params_list[[.]], CARD = cards[.]))
# params_to_update <- sample(wq_params$PARAM, size = 5)
# update_wq_params_value('KHNc', '999', inp = inp)
utils::globalVariables(c('PARAM', 'CARD'))