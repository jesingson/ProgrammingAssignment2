## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix creates a wrapper around a matrix object that defines
## a series of functions to get/set the matrix and get/set its inverse
## This wrapper object will be used by the cacheSolve() function
## which manipulates the object to solve the matrix inversion and cache it
## in this object or return the cached inverse (stored in inv) if 
## already available.

makeCacheMatrix <- function(x = matrix()) {
  # inv: variable containing the cached inverse
  inv <- NULL
  
  # Define four sets of functions -- 
  # 1) set the matrix and delete cached object since the matrix values 
  # have changed
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  # 2) get the original matrix 
  get <- function() x
  # 3) setInverse: store the cached inverse in this object
  setInverse <- function(mtx) inv <<- mtx
  # 4) getInverse: retrieve the cached inverse from this object
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve attempts to first retrieve the inverse from the 
## CacheMatrix object if it's already been set
## if this is not yet set, then calculate the inverse and
## cache it into the CacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The CacheMatrix object doesn't have it cached. Let's do the
  # solve and cache the results in the CacheMatrix object
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  # Return the calculated inverse
  inv
}
