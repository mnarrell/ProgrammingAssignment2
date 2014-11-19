## Programming Assignment 2
## Two functions used to create a special object that stores a matrix and 
## cache's its inverse.

## Creates a matrix container capable of getting/setting a matrix and 
## its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  # The inverse of the prescribed matrix.
  inverse <- NULL
  # Set the value of the matrix, and empty the cached inverse.
  set <- function(newMatrix) {
    matrix <<- newMatrix
    inverse <<- NULL
  }
  # Gets the matrix.
  get <- function() {
    matrix
  }  
  # Sets the inverse.
  setInverse <- function(solved) {
    inverse <<- solved
  }  
  # Gets the inverse.
  getInverse <- function() {
    inverse
  }  
  # I'm not sure of this syntax.  Seems like a object-oriented notation 
  # to allow for $ property access?
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Computes the inverse of a given cacheable matrix "x".  In the case of a 
## cache miss, the inverse is calculated and stored.
cacheSolve <- function(matrixContainer, ...) {
  # Interrogate the cache.
  inverse <- matrixContainer$getInverse()
  if(!is.null(inverse)) {
    # Cache hit, return the cached data.
    message("cache hit!")
    return(inverse)
  }
  # Cache miss.  Calculate the inverse, cache it, and return the inverse.
  message("cache miss!")
  matrix <- matrixContainer$get()
  inverse <- solve(matrix, ...) # Forwards parameters to solve via "..."
  matrixContainer$setInverse(inverse)
  inverse
}
