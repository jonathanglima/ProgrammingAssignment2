## Below are the functions that together can inverse a matrix and 
## cache it's result to resolve it faster

## This function keeps the environment of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## This is the function that inverses the matrix using the function solve, but first it tries
## to read the matrix data from the cache

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
