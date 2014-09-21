## These two functions make it possible to have a matrix cache its inverse
## so we don't have to recompute it if the matrix hasn't changed. Upon
## matrix change, the inverse will be recomputed.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv=x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached inverse.")
    return(inv)
  }
  ## If there is no cached inverse, compute inverse and update matrix object:
  mtrx <- x$get()
  inv <- solve(mtrx)
  x$setinverse(inv)
  inv
}
