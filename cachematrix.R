## The following functions create a matrix and computes its inverse;
## if the latter computation has already been made, the computation 
## is saved (cached) so that the process does not need to be repeated

## makeCacheMatrix creates a matrix and caches its inverse if it has
## been computed

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(my.matrix) m <<- my.matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## cacheSolve computes the inverse of a matrix; but first it checks
## to see if the computation has already been made

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m  
}


