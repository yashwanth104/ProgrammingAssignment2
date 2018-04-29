## This is demonstration of using cache for avoiding repitative 
##calculations for unchanged values in loops

## This function creates special "matrix"
##set the value of the matrix
##get the value of the matrix
##set the value of the matrixInverse
##get the value of the matrixInverse

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


## This function returns the inverse matrix from cahe if it exists, 
##if not it calculates the inverse and stores 

cacheSolve <- function(x, ...) {
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