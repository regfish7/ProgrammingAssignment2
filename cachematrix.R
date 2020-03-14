## These functions will cache the inverse of a matrix so that it can be used in later
## computation. 

## This function creates a matrix object of a given matrix to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m 
  list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}


## This function computes the inverse of the matrix object returned by makeCacheMatrix.
## If the inverse has already been calcuated, then this function retrieves the inverse
## from the cache. Otherwise, it caculates the inverse and saves it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
