## Functions developed to cache calculations of matrix inverses to decrease run time

## Function creates a vector made up of other functions to enable user to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function returns inverse of matrix set in makeCacheMatrix.  If inverse exists, pulls 
## from cache, otherwise calculates and saves to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
