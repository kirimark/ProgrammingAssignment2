##These two functions use lexical scoping to solve and cache the inverse of a matrix.
##makeCacheMatrix creates an object to be passed to cacheSolve. 


##This function creates an R object that can store a matrix and its inverse.
##Returns a list of type makeCacheMatrix to be used by cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function retrieves the cached value of the inverse. If this value is NULL, it solves
##the inverse and caches that value. I
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)   
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}