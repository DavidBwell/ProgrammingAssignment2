# Two function makeCacheMatrix() and cacheSolve() take a square matrix, solve for its inverse and allow the inverse to be stored 
# so that it can be called from memory rather than computed when needed subsequently.

#makeCacheMatrix receives takes a matrix as its argument. set and get functions allow for arguments to be passed and returned 
#after instantiation.
makeCacheMatrix <- function(x = matrix()) {
  
  aMatrix <- matrix(NA) ## creates a matrix of NA values when a user does not pass arguments on instantiation. 
  
  # allows the user to set the values for a matrix y.  In case y is passes as a vector, Z transforms it to a square matrix.     
  set <- function(y){
    z <- matrix(y,sqrt(length(y)), sqrt(length(y)))  
    x<<-z
    aMatrix <<- matrix(NA, sqrt(length(y)), sqrt(length(y))) 
  }
  
  get <- function() x # takes the value from set and super assigns it for reference in other environments
  setInverseMatrix <- function(x) aMatrix <<-solve(x)  # uses the get() value of x and solves for (sets) its inverse.
  
  getInverseMatrix <- function() aMatrix ## returns the inverse of X determined in the setMatrix function.
  
  # returns a list that is the final output of the makeCache Matrix function. The return makes these functions available in the global env. 
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)   
}


## The cacheSOlve takes the information available from the makeCacheMatrix function in order to return the inverse 
## of matrix X through an assignment to a variable. M  <-cacheSolve() assingns the inverse of matrix X in the matrix M.

cacheSolve <- function(x, ...) {
  
  anInverseMatrix <- x$getInverseMatrix()  #retrieves the value of anInverseMatrix from the getInverseMatrix function in makeCacheMatrix()
  # checks that the inverseMatrix function is not returning the default matrix created in makeCacheMatrix() indicates that it is not and returns the inverse, 
  if(!is.na(anInverseMatrix[1,1])){
    print("It looks like this program is going to work!")
    return(anInverseMatrix)
  }
  anInverseMatrix <- x$get() # retrieves the value of the inverse matrix of x in the makeCacheMatrix function
  inverseOfAMatrix <-solve(anInverseMatrix, ...) # solves for the inverse of aMatrix from the cacheSolve function.
  #x$setInverseMatrix(inverseOfAMatrix)
  inverseOfAMatrix # returns the value of the solution for inverse of Matrix X
  
}