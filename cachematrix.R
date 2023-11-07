## These functions take a matrix and create a vector which can be manipulated by the cacheSolve function
## in order to find the inverse of the matrix, assuming a square invertible matrix.

## Creates a vector of the functions to set the original matrix, get it, set the inverse matrix, and
## get the inverse matrix, storing all data in the cache.

makeCacheMatrix <- function(x = matrix()) {
  xneg <- matrix()
  set <- function(y) {
      xneg <<- matrix()
      x <<- y
  }
  get <- function() {
    x
  }
  setMatrix <- function(matrix = x, ...) {
    xneg <<- solve(matrix, ...)
  }
  getMatrix <- function() {
    xneg
  }
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

## Takes the output of the function above, as well as any other matrix solution inputs, and
## outputs the inverse matrix from the cache

cacheSolve <- function(x, ...) {
  test <- x$getMatrix()
  if(all(!is.na(test))) {
    return(test)
  }
  matrix <- x$get()
  x$setMatrix(matrix)
  newMatrix <- x$getMatrix()
  newMatrix
}
