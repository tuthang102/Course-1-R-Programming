rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcome_type = c('heart attack', 'heart failure', 'pneumonia')
  if (is.element(state, outcome_data$State) == FALSE){
    stop('invalid state')
  }
  else if (is.element(outcome, outcome_type) == FALSE) {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data = outcome_data[which(outcome_data$State == state), ]
  data = data[,c(2,7,11,17,23)]
  data[3:5] = sapply(data[3:5], as.numeric)
  if (outcome == 'heart attack') {
    data = data[which(!is.na(data[,3])),]
    index = order(data[,3], data$Hospital.Name)
  }
  else if (outcome == 'heart failure') {
    data = data[which(!is.na(data[,4])),]
    index = order(data[,4], data$Hospital.Name)
  }
  else if (outcome == 'pneumonia'){
    data = data[which(!is.na(data[,5])), ]
    index = order(data[,5], data$Hospital.Name)
  }
  result = data$Hospital.Name[index]
  if (num == 'best') {
    return(result[1])
  }
  else if(num == 'worst') {
    return(result[length(result)])
  }
  else if ( num > length(result)) {
    return(NA)
  }
  else {
    return(result[num])
  }
}
