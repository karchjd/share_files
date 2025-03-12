extract_marginal_means <- function(x){
  array(x$fit, dim = sapply(x$variables, function(x) length(x$levels)), 
        dimnames = lapply(x$variables, function(x) x$levels))
}