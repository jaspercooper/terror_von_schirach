
make_data <- function(cutpoint,window){
  data <- subset(
    x = terror,
    subset = 
      (date <= (as.Date(cutpoint) + window)) & 
      (date >= (as.Date(cutpoint) - window)) 
  )
  return(data)
}

