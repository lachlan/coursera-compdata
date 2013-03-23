corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  source("getmonitor.R", local = TRUE)
  
  #ids <- as.list(subset(complete(directory), nobs >= threshold, id))  
  #data <- for (id in ids) {
  #  getmonitor(id, directory)
  #}
  
  m <- lapply(1:332, function(id, directory) {
    data <- na.omit(getmonitor(id, directory))
    if (nrow(data) > threshold) {
      data
    } else {
      NULL
    }
  }, directory = directory)
  
  results <- numeric(0)
  
  for (i in m) {
    if (!is.null(i)) {
      result <- cor(i$nitrate, i$sulfate)[1]
      if (!is.na(result)) results <- c(results, result)
    }
  }
  
  results
}