## This function creates a special "matrix" object that can cache its inverse. 
## The function creates a list containing the following functions:
## set: to assign the value of the matrix
## get: to retrieve the value of the matrix
## setInverse: to set the value of the inverse matrix
## getIverse: to retrieve the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # Initialise the value of the inverse matrix
  inv <- NULL
  
  # Set the value of the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL   # ensure the value of the inverse is initialised
  }
  
  # Retrieve the value of the 
  get <- function() x
  
  # Set the inverse martix
  setInverse <- function(invIn) inv <<- invIn
  
  # Retrieve the inverse matrix
  getInverse <- function() inv
  
  # Return a list of the functions: set, get, setInverse, getInverse
  list (set = set, get = get, 
        setInverse = setInverse,
        getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
## Alternatively, if the inverse is not in the cache then it calculates the
## inverse of the matrix.

cacheSolve <- function(x, ...) {
  # Retrieve the value of the inverse matrix
  inv <- x$getInverse()
  
  # If the inverse has been cached, return the value from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    
    # return the inverse and exit the function
    return (inv)
  }
  
  # If the inverse is not in the cache,
  # retrieve the matrix
  data <- x$get()
  
  # calculate the inverse
  inv <- solve(data, ...)
  
  # set the inverse in the cahce
  x$setInverse(inv)
  
  #return the inverse
  inv
}
