## Put comments here that give an overall description of what your
## functions do

## 
# The makeCacheMatrix function returns a list of 4 functions that 
# set the value of the matrix
#get the value of the matrix
#set the inverse of a matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function returns the Inverse of a function.
#It first checks if the Inverse of the inverse is already available. If it is
# it returns the cahced Inverse. If not it uses the Solve function to calculate the
# Inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
