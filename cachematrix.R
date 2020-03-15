## These functions will cache the inverse of a matrix so that it can be used in later
## computation. 

## This function creates a matrix object of a given matrix to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  #initialize the cached matrix
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  #Set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  #Get the value of the inverse
  getinverse <- function() m 
  list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}


## This function computes the inverse of the matrix object returned by makeCacheMatrix.
## If the inverse has already been calcuated, then this function retrieves the inverse
## from the cache. Otherwise, it caculates the inverse and saves it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #Check to see if the inverse has already been calculated/cached
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #Calculate the inverse 
  data <- x$get()
  m <- solve(data, ...)
  #Set the inverse in the cache
  x$setinverse(m)
  m
}
