## One function create a list of functions that will be used in the second function
##

## makeCacheMatrix return a list of functions 
## set function sets x to the matrix y from the input and reset the inverse to NULL
## get returns the cache matrix
## setinverse sets the inverse to the input of the function
## getinverse return the cache inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the cache of the inverse of the matrix (if there is any)
## if the matrix i is absent it calls get() to get the matrix and solve for 
## the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
