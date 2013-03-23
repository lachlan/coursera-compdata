agecount <- function(age = NULL) {
  library(stringr)
  
  if (is.null(age)) stop("invalid cause specified: NULL")
  
  homicides <- readLines("homicides.txt")
  
  match <- str_match(homicides, "([0-9]+) years old")
  results <- table(na.omit(tolower(match[,2])))
  
  result <- results[names(results)==as.numeric(age)]
  names(result) <- NULL
  
  if (length(result) == 0) result <- 0
  
  return(result)
  
}