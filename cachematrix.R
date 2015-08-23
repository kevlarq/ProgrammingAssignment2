## Assigned task is to cache the inverse of a matrix:
## Reason is matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Create a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse by performing the following steps:
##    1. 	set the value of the matrix
##    2.	get the value of the matrix
##    3.	set the value of the inverse
##    4.	get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## use of '<<-' to assign a value to an object residing a different env. other than the current env.
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the created by the makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}