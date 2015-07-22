
## the first function, makeCacheMatrix, creates a closure which can used
## to memoize or cache the inverse of a matrix.  This technique
## increases efficiency by preventing multiple execution of the same code.
##=================================================================================

## makeCacheMatrix is a closure that contains the state variables for a matrix "m"
## and its inverse "inv".
## the closure also contains methods for getting and setting the state of m and inv
##=================================================================================
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is passed a closure from makeCacheMatrix
## if the inverse has already been set, it returns that cached value
## otherwise, it calculates the inverse, stores that value in the closure, 
## and returns that value
##=================================================================================
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

## sample code
## m1 <- matrix(c(2, 1, 5, 3), nrow=2, ncol=2)
## mcm <- makeCacheMatrix(m1)
## > cacheSolve(mcm)
## [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
## > cacheSolve(mcm)
## getting cached data
## [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
