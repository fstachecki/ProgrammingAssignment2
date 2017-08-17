## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
