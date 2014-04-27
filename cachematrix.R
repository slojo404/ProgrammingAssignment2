## A set of functions to calculate the inverse of a matrix such that the results
## are cached.  In this way long calculations can be pulled from the cache instead
## of recalculated.

## This function wraps the passed in valid square matrix with the scoped functions
## needed to cache the results of the inverse computation.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(the_inverse) m <<- the_inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function uses the scoped functions that are part of the special "cache matrix" 
## we created from the makeCacheMatrix function, to either retrieve the inverse from
## cache if it is already there or calculate the inverse using solve and cache the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
