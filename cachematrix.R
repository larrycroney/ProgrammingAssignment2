
## This program provides the ability to compute the inverse of a matrix,
## using cached results if available.


## makeCacheMatrix converts an input matrix into a cache-ready closure
## including getter and setter functions.

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


## cacheSolve takes the closure created above as input, and if
## the inverse is cached, gets that, otherwise calculates the
## inverse and sets it into cache.

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


# test
# m <- matrix(runif(1000000), 1000, 1000)
# mClosure <- makeCacheMatrix(m)
# cacheSolve(mClosure) # first run is slower than subsequent runs: it works

