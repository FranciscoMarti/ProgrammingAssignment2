## Put comments here that give an overall description of what your
## functions do

## This function returns a S3 object with getters and setters for
##     the matrix provided and its inverse (calculated in cacheSolve)
## Same as makeVector in the example with vector mean,
##     only symbol names are updated

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if an object has the inverse of the associated
##    matrix cached; if not, it solves and caches the inverse of the matrix
## Almost the same as cachemean in the example with vector mean;
##    it uses solve() for the inversion and also checks if
##    the matrix provided is invertible (although not required)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  data_determinant <- try(det(data), silent = TRUE)
  if (is.numeric(data_determinant)) {
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
  }
  else {
    message("not a square invertible matrix")
    invisible(NULL)
  }
}

