## The makeCacheMatrix and cacheSolve functions can solve the
## inverse of a matrix and cache the result. This speeds up
## the calculations in the future as matrixInversion is expensive

## Creates a custom matrix with mutliple functions to get and set 
## set the matrix/inverse. (Basically a class in PHP/JS)

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse 
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## Actually solves the matrix. Uses appropriate functions defined
## in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
