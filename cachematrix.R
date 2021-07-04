## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. The following pair of functions can cache the inverse of a matrix.

## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.
##                   It returns a list containing 4 functions which do the following:
##                         set the value of the vector
##                         get the value of the vector
##                         set the value of the mean
##                         get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##              If the inverse has already been calculated (and the matrix has not changed),
##              then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinv(inv)
  inv
}