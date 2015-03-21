##This file contains two functions which work together to return the inverse of an invertable matrix.
##The first function creates a matrix object which can cache the inverse of the supplied matrix.
##Assign makeCachMatrix with an invertable matrix argument to a variable, and supply this variable
##as the argument in cacheSolve.
##cacheSolve returns the inverse from the cache or computes the inverse if it is not stored in the cache.

##makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
  inv <- NULL
  set <- function(y) {
    mtrx <<- y
    inv <<- NULL
  }
  get <- function() mtrx
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated, then cacheSolve 
##retrieves the inverse from the cache.  If the matrix has changed, you must run makeCacheMatrix first.

cacheSolve <- function(mtrx, ...) {
    
  inv <- mtrx$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mtrx$get()
  inv <- solve(data, ...)
  mtrx$setinv(inv)
  inv
  }