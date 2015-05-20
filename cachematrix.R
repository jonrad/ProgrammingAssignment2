## Helper functions to give R the ability to solve the inverse
## of a matrix and cache the results

## Creates a wrapper around a matrix that allows for caching
## of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(value) inverse <<- value
  
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Uses the special cache matrix object to solve inverses
## with caching

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)
  x$setInverse(inverse)
  
  inverse
}
