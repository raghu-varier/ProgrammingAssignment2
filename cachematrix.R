
## Put comments here that give an overall description of what your
## functions do
## Functions to first create matrix object and cache its inverse so as to 
## if the matrix has not changed then will return the inverse from cache

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) m <<- matinv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
## This function computes the inverse of matrix 
## returned by above function.
## And if the inverse is already calculated then the inverse
## is returned from cache.

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
## Return a matrix that is the inverse of 'x'

