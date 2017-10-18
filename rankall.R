rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcome_type = c('heart attack', 'heart failure', 'pneumonia')
  if (is.element(outcome, outcome_type) == FALSE) {
    stop('invalid outcome')
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  state_factor = as.factor(outcome_data$State)
  hospital_name = c()
  for (state in levels(state_factor)) {
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
    hos_name_sorted = data$Hospital.Name[index]
    if (num == 'best') {
      result = hos_name_sorted[1]
    }
    else if(num == 'worst') {
      result = hos_name_sorted[length(index)]
    }
    else if (num > length(index)) {
      result = NA
    }
    else {
      result = hos_name_sorted[num]
    }
    hospital_name = c(hospital_name, result)
  }
  final_answer = data.frame(hospital = hospital_name, state= levels(state_factor))
}