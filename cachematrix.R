## Put comments here that give an overall description of what your
## functions do

## create matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  
  getInverse <- function inverse
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## compute inverse, pulling from cached if it exists, otherwise calculating it & caching it

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data)
  
  x$setInverse(inverse)
  
  inverse
}
