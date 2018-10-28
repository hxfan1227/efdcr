#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
NULL
#' Tidy the water level data
#' @param x Vector of water level
#' @return Vector of formated water level
tidy_water_level2 <- function(x){
  dot_idx <- str_detect(x, '\\.')
  integer_part <- str_split_fixed(x, '\\.', 2)
  integer_to_add <- transformed <- array(0, dim = length(x))
  transformed[!dot_idx] <- as.numeric(x[!dot_idx])/100
  transformed[dot_idx] <- as.numeric(x[dot_idx])
  for (i in (1:length(dot_idx))){
    if (dot_idx[i]){
      int_part <- integer_part[i]
      integer_to_add[i] <- 0
    } else{
      integer_to_add[i] <- int_part
    }
  }
  return(as.numeric(integer_to_add) + transformed)
}