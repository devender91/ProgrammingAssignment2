## Our main goal is to write a pair of functions that cache the inverse of a matrix.
## Matirx inversion is usually a costly computation and there may be some benefit to caching
## the inverse if the matrix rather than computing it repeatedly
## Below functions create a special object  that stores a matrix and caches its inverse.

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set,get = get, setmean= setmean, getmean = getmean)
  
}
## cacheSolve creates the inverse of the special matrix created by makeCacheMatrix above. 
## If the inverse has already been calculated then it will retrive the inverse from the cache. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
  
}
