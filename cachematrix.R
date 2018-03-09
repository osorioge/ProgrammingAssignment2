## create a matrix, create a free variable, set the function for "y", create a value in the cache memory, return x value, set the inverse function
## function inverse the matrix 

## inverse the matrix x 

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


## if the value mi is not null go to the cache and get the value, in this case "mi" which is the solve of the matrix "x"

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
