corr <- function(directory, threshold = 0) {
  completed_cases=dim(complete(directory))[1]
  v=c()
  if(completed_cases>threshold)
  {
    v=corr()
    
  }
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}