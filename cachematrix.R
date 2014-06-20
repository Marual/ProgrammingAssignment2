## makeCacheMatrix is a special function that can keeps its inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve gives the inverse if the result don't change.

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
        ## Return a matrix that is the inverse of 'x'
}
