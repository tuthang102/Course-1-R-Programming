### PART 1
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# outcome[, 11] <- as.numeric(outcome[, 11])
# hist(outcome[, 11])

### PART 2

best <- function(state, outcome) {
  ## Read outcome data
  outcome_data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid'
  outcome_type = c('heart attack', 'heart failure', 'pneumonia')
  if (is.element(state, outcome_data$State) == FALSE){
    stop('invalid state')
  }
  else if (is.element(outcome, outcome_type) == FALSE) {
    stop('invalid outcome')
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data = outcome_data[which(outcome_data$State == state), ]
  if (outcome == 'heart attack'){
    data[,11] = as.numeric(data[,11])
    data = data[which(!is.na(data[,11])), ]
    index = order(data[,11], data$Hospital.Name)
  }
  else if (outcome == 'heart failure') {
    data[,17] = as.numeric(data[,17])
    data = data[which(!is.na(data[,17])), ]
    index = order(data[,17], data$Hospital.Name)
  }
  else if (outcome == 'pneumonia') {
    data[,23] = as.numeric(data[,23])
    data = data[which(!is.na(data[,23])), ]
    index = order(data[,23], data$Hospital.Name)
  }
  return (data$Hospital.Name[index][1])
}
