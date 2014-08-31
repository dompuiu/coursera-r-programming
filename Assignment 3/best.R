best <- function(state, outcome) {
  ## Read outcome data
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Find state codes
  state_codes = unique(data[,'State'])
  possible_outcomes = c("heart attack", "heart failure", "pneumonia")
  
  ## Create outcome data frame
  ## Filter and simplify the column names
  data = data[c(2, 7, 11, 17, 23)]
  names(data) = c("name", "state", possible_outcomes)
  
  ## Check that state and outcome are valid
  if (!is.element(state, state_codes)) {
    stop("invalid state")
  }
  
  if (!is.element(outcome, possible_outcomes)) {
    stop("invalid outcome")
  }
  
  ## Grab only rows with our state value
  data = data[data$state==state & data[outcome] != 'Not Available', ]
  row_index = which.min(data[, outcome])
  
  ## Return hospital name in that state with lowest 30-day death rate
  data[row_index, ]$name
}