NULL
#' Calculate the Nash-Sutcliffe efficiency (NSE)
#' @param obs Numeric vector of matrix. Measured values
#' @param sim Numeric vector of matrix. Predicted values
#' @return The Nash-Sutcliffe efficiency (NSE)
#' @references Nash, J. E., and J.V. Sutcliffe (1970): River flow forecasting through conceptual models. 1. a discussion of principles. Journal of Hydrology 10, 282–290.
#' @export
nse <- function(obs, sim, na.rm = T) {
  x <- obs
  y <- sim
  if (!check_dim_(x, y)) {
    stop('inputs are not same in dim: dim(x) = (', NROW(x), ',', NCOL(x), ')', ' but dim(y) = (', NROW(y), ',', NCOL(y), ')')
  }
  if (na.rm) {
    x <- na.omit(obs)
    y <- na.omit(sim)
  }
  if (NCOL(x) == 1)
    return(1 - ((sum((x - y)^2))/(sum((x - mean(y))^2))))
  else 
    return(1 - ((colSums((x - y)^2))/(colSums((x - colMeans(y))^2))))
}

# check dim
# 
#check_dim_ <- function(...){
#  arg_list <- list(...)
#   arg_class <- unlist(lapply(arg_list, class))
#   if (!all(arg_class == arg_class[1])) {
#     stop('Inputs are not with same class!')
#   }
#   arg_dim <- lapply(arg_list, dim)
#   if (all(unlist(lapply(arg_dim, is.null)))) {
#     arg_len <- unlist(lapply(arg_list, length))
#     return(all(arg_len == arg_len[1]))
#  } else {
#     return(all(unlist(lapply(arg_dim, function(x){all(x == arg_dim[[1]])}))))
#  }
#   
#}

#' Check if all the inputs are same in dimensions
#' @param ... Objects to be checked
check_dim_ <- function(...){
  arg_list <- list(...)
  arg_class <- unlist(lapply(arg_list, class))
  if (!all(arg_class == arg_class[1])) {
    stop('Inputs are not with same class!')
  }
  arg_dim <- lapply(arg_list, function(x){c(NROW(x), NCOL(x))})
  return(all(unlist(lapply(arg_dim, function(x){all(x == arg_dim[[1]])}))))
}

