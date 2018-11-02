## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## The functions below are used to create/stores a matrix
## and caches it's inverse.

## Creation of a special "matrix" object that can cache it's own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
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


## Used to compute the inverse of the makeCacheMatrix funciton.
## If the inverse has already been calculate, then it will retrieve the exact inverse
## from the cache. 

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
