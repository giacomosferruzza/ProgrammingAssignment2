## The following functions are able to retrieve the inverse of a matrix from the cached values to avoid repeating time-comsuming computations 

## makeChacheMatrix creates a list of functions required by the cacheSolve and stores a matrix and its inverse once calculated by cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve firstly checks if the inverse of the matrix has already been calculated. If so, it gets the result form the cache. Otherwise it calculates the inverse of the matrix and sets in the cache. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
