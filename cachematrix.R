## I am going to write pair of functions which will be able to cache the inverse of a matrix.
## I used the makeVector function as a framework
## makeCacheMatrix function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
  x <<- y
  k <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## I used the cachemean function as a framework
## cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix
## cacheSolve will retrieve the inverse from the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
  k <- x$getInverse()
  if(!is.null(k)){
  message("getting cached data")
  return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
