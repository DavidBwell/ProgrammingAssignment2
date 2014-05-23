
# This function takes user input for a matrix X and calculates its inverse.  The values of X and its inverse are returned to the global environment.

makeCacheMatrix <- function(x = matrix()) {
  anInversematrix <-matrix() ## creates an empty matrix as a reference for within the function

# allows the user to set the values for a variable y.  The y-values are then transfered to a square matrix.  A default inverse Matrix is also calculated.   
set < function(y){
      z <- matrix(y,sqrt(length(y)), sqrt(length(y)))  
      x<<-z
      anInverseMatrix <<- matrix(NA, sqrt(length(y)), sqrt(length(y))) 
  }

get <- function() x # takes the value from set and super assigns it for reference in other environments
setInverseMatrix <- function(solve) anInversematrix <<-solve(x)  # uses the get() value of x and solves for (sets) its inverse.

getInverseMatrix <- function() anInversematrix ## returns the inverse of X determined in the setMatrix function.

# returns a list that is the final output of the makeCache Matrix function. The return makes these functions available in the global env. 
list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)   
}


## The cacheSOlve takes the information available from the makeCacheMatrix function in order to return the inverse 
## of matrix X through an assignment to a variable.  z <-cacheSolve() assingns the inverse of matrix X in the variable z.

cacheSolve <- function(x, ...) {
        
  anInverseMatrix <- x$getInverseMatrix()  #retrieves the value of anInverseMatrix from the getInverseMatrix function in makeCacheMatrix()
  # checks that the inverseMatrix function is not returning the default matrix created in makeCacheMatrix() indicates that it is not and returns the inverse, 
  if(!is.na(anInverseMatrix[1,1])){
     print("It looks like this program is going to work!")
     return(anInverseMatrix)
   }
  anInverseMatrix <- x$getInverseMatrix() # retrieves the value of the inverse matrix of x in the makeCacheMatrix function
  inverseOfAMatrix <-solve(anInverseMatrix, ...) # solves for the inverse of aMatrix from the cacheSolve function.
  inverseOfAMatrix # returns the value of the solution for inverse of Matrix X
  
}
