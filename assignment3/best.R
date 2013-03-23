best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv")
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  indices <- c(11, 17, 23)
  
  if (!state %in% data$State) stop("invalid state")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  i <- indices[match(outcome, outcomes)]
  hospitals <- data[data$State == state, c(2, i)]
  hospitals[, 2] <- as.numeric(as.character(hospitals[, 2]))
  hospitals <- na.omit(hospitals)
  names(hospitals) <- c("name", "deaths")
  min_deaths <- min(hospitals$deaths)
  candidates <- hospitals[hospitals$deaths == min_deaths, ]$name
  return(as.character(sort(candidates)[1]))
}