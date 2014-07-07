# Self-made or adapted functions that might come in handy later

StErr <- function(x) {  # Takes data.frame (columns) as input, gives a vector of st.error of each column as output
  sqrt(var(x,na.rm=TRUE)/sum(!is.na(x)))
}

table.plot <- function(x) {  #Takes array as input, returns data.frame ready for plotting
  
  StErr <- function(x) {  # Standart error
    sqrt(var(x,na.rm=TRUE)/sum(!is.na(x)))
  }
  means <- colMeans(x, na.rm=TRUE)
  stErr <- apply(x, 2, StErr)
  stErr.UP <- means + stErr
  stErr.DN <- means - stErr
  sub.x <- data.frame(means,stErr, stErr.UP, stErr.DN)
  return(sub.x)
}