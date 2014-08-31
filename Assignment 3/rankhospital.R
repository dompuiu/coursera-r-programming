rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  state_codes = unique(data[,'State'])
  possible_outcomes = c("heart attack", "heart failure", "pneumonia")
  
  if (!is.element(state, state_codes)) {
    stop("invalid state")
  }
  
  if (!is.element(outcome, possible_outcomes)) {
    stop("invalid outcome")
  }
  
  ## Validate the num value
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  ## Create outcome data frame
  ## Filter and simplify the column names
  data = data[c(2, 7, 11, 17, 23)]
  names(data) = c("name", "state", possible_outcomes)
  
  ## Grab only rows with our state value    
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  
  ## Order the data
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ## Process the num argument
  vals <- data[, outcome]
  if( num == "best" ) {
    rowNum <- which.min(vals)
  } else if( num == "worst" ) {
    rowNum <- which.max(vals)
  } else {
    rowNum <- num
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  data[rowNum, ]$name
}