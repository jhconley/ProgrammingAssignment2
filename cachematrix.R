## A pair of functions that support the caching
## of matrix inversions: one creating an object 
## to store a matrix and its inversion;
## the other to compute-and-save or retrieve
## the inversion using this object.

## a function that creates a matrix container  
## that can cache the matrix inverse

makeCacheMatrix <- function(m = matrix()) {
  mi <- NULL
  set <- function(y) {
    m <<- y
    mi <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) mi <<- inverse
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## a function that returns the inverse of a given matrix.
## if the matrix container has the inverse, use it; 
## otherwise, compute the inverse and store the result 
## in the container before returning it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  m <- x$get()
  if (det(m) != 0) {
    mi <- solve(m)
    x$setinverse(mi)
  } else { ## the matrix is singular
    ## e.g. matrix(1:100, c(10,10)) 
    ##          message("the matrix is not invertible")
  }
  mi
}

