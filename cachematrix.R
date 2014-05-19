## These functions provide the ability to store a matrix 'x'
## and its inverse so that the inverse of 'x' only needs to
## be computed once.

## Use makeCacheMatrix to construct an object 'M' which holds
## a matrix 'x' and can store its inverse 'inv' via cacheSolve
makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Use cacheSolve to compute the inverse of 'x'. If the inverse
## has previously been computed, then cacheSolve will retrieve
## the inverse from 'M'.
cacheSolve <- function(M, ...) {
  inv <- M$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data...")
    inv
  }
  inv <- solve(M$get())
  M$setinverse(inv)
  inv
}