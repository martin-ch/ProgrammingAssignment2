## Taking the inverse of a matrix can be a resource intensive operation.
## Below function allow the caching of the inverse of a matrix


## The function "makeCacheMatrix" prepares the necessary functions to store and retrieve a matrix and its inverse  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" returns the inverse of the matrix stored the object created by "makeCacheMatrix".
## In case the inverse was already calculated it will return the cached inverse.

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
