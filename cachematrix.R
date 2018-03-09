## create a matrix, create a free variable, set the function y for create a value in the cahe memory, return x value 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mi <<- inverse
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinverse(mi)
  mi
}
