
## This program provides a method to compute the inverse of a matrix,
## using cached results if available.


## makeCacheMatrix converts an input matrix into a "cache-ready"
## object including getter and setter functions.

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


## cacheSolve takes the object created above as input, and if
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
# cacheSolve(makeCacheMatrix(matrix(1:4, 2, 2)))

