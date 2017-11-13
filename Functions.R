##########
# Functions script to make main code more readable
##########

# Create an unscale function to help interpret scaled data.
unscale <- function(in.vec) {
  if(is.null(attr(in.vec, 'scaled:scale')==FALSE)) {
    return(in.vec * attr(in.vec, 'scaled:scale') + attr(in.vec,'scaled:center'))
  } else {
    return(in.vec)
  }
  
}

rmse <- function(trueVec,predVec) {
  error <- trueVec - predVec
  return(sqrt(mean(error^2)))
}

mae <- function(trueVec,predVec) {
  error <- trueVec - predVec
  return(mean(abs(error)))
}
