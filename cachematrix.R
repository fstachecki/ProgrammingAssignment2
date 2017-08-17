## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      theInverse <- NULL
      set <- function(y) {
            x <<- y
            theInverse <<- NULL
      }
      
      get <- function() x
      setsolve <- function(solve) theInverse <<- solve
      getsolve <- function() theInverse
      
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      theInverse <- x$getsolve()
      if(!is.null(theInverse)) {
            message("getting cached data")
            return(theInverse)
      }
      data <- x$get()
      theInverse <- solve(data, ...)
      x$setsolve(theInverse)
      theInverse
}
