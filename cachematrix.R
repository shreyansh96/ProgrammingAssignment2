## This program consists of two functions makeCacheMatrix() and cacheSolve() which are 
## used to calculate the inverse of a matrix and to cache it to make the computation faster.

## The below function makeCacheMatrix is used to create a special "Matrix object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The below function cacheSolve is used to compute the inverse of special matrix object 
## returned by the above function. Also this function checks that if the inverse is already 
## calculated or not and thereby returning the cached inverse or computing the inverse and
## storing it.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
