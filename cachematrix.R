## Two functions designed to make an invertable matrix and to save the inverse in the 
## parent environment so that computation time to compute the inverse on subsequent calls.

## makeCacheMatrix creates a square matrix and provides subfunctions that allow its values to be 
## set and retrieved in a parent environment

makeCacheMatrix <- function(x = matrix()) {
  amatrix <-matrix() ## creates an empty matrix as a reference for within the function

##  The set function takes external values for y and converts a vector into the matrix z.  Z has dimension 
#   such that if y is a vector of length n^2, there are n rows and n columns of z.
#   x is the z-matrix super assigned to the parent environment so that x is available was the function
#   stack is popped off.  aMatrix is a square matrix of NA values sent to the parent environment.  It is filled later.
  set <- function(y){
      z <- matrix(y,sqrt(length(y)), sqrt(length(y)))  
      x<<-z
      aMatrix <<- matrix(NA, sqrt(length(y)), sqrt(length(y))) 
  }
# The get function is called outside the function.  It alows the user to get the matrix values super assigned 
#  to x
get <- function() x
setMatrix <- function(solve) aMatrix <<-solve  # takes the value for aMatrix.  
                                                      #  Then super assigns it to be called outside the makecacheMatrix function

getMatrix <- function() aMatrix ## a reference to aMatrix so it can be returned in the list below

list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix) # returns a list that is the final output of the makeCache Matrix function.  
                                                                         # the list is returned to the global environment.  
}


## The cacheSOlve takes the information available from the makeCacheMatrix function in order to return the inverse 
## of aMatrix.

cacheSolve <- function(x, ...) {
        
  aMatrix <- x$getMatrix()  #retrieves the value of aMatrix from the getMatrix function in the previous command
   if(!is.na(aMatrix[1,1])){
     print("It looks like this program is going to work!")
     return(aMatrix)
   }
  aMatrix <- x$get() # retrieves the value of matrix x in the makeCacheMatrix function
  inverseOfAMatrix <-solve(aMatrix, ...) # solves for the inverse of aMatrix from the cacheSolve function.
  inverseOfAMatrix # returns the value of the solution for inverse of aMatrix
  
}
