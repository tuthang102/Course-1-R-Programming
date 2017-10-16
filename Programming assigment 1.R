## PART 1

pollutantmean <- function (directory, pollutant, id=1:332) {
  setwd(paste('C:/Users/Orpheus/Desktop/Coursera/R Programing/', directory, sep = ''))
  total = c()
  for (i in seq_along(id)) {
    if (id[i] <= 9) {
      data = read.csv(paste('00', id[i], '.csv', sep ='')) 
    }else if (10 <= id[i] &  id[i] < 100 ) {
      data = read.csv(paste('0',id[i], '.csv', sep ='' ))
    }else {
      data = read.csv(paste(id[i], '.csv', sep = ''))
    }
    non_NA = na.omit(data[pollutant])
    total <- c(total, non_NA[[pollutant]])
  }
  result = mean(total)
  result
}

## PART 2

complete <- function(directory, nid=1:332) {
  setwd(paste('C:/Users/Orpheus/Desktop/Coursera/R Programing/', directory, sep = ''))
  df <- data.frame(id = character(length(nid)), nobs = character(length(nid)), stringsAsFactors = FALSE)
  for (i in seq_along(nid)) {
    if (nid[i] <= 9) {
      data = read.csv(paste('00', nid[i], '.csv', sep ='')) 
    }else if (10 <= nid[i] &  nid[i] < 100 ) {
      data = read.csv(paste('0',nid[i], '.csv', sep ='' ))
    }else {
      data = read.csv(paste(nid[i], '.csv', sep = ''))
    }
    num_ob = nrow(na.omit(data))
    df$id[i] = nid[i]
    df$nobs[i] = num_ob
  }
  df
}

## PART 3

corr <- function(directory, threshold = 0){
  setwd(paste('C:/Users/Orpheus/Desktop/Coursera/R Programing/', directory, sep = ''))
  result = c()
  for (name in list.files(pattern = 'csv')) {
    data = read.csv(name)
    data = na.omit(data)
    if (nrow(data) > threshold) {
      result = c(result, cor(data$nitrate, data$sulfate))
    }
  }
  result
}  
