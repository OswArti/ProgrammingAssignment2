## This code cache the inverse of  matrix, by using 
## two functions: makeCacheMatrix and cacheSolve.
## It is assumed that the matrix is real, square, 
## and invertible. If the matrix is singular 
## the program will abort with an error message:
##  Error in solve.default(data, ...) : 
## Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
###############################################################

## This function has a real, square, invertible  matrix 
## as input.
## The output of the function  is a list with the 
## following four functions:
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinver <- function(inver) inv <<- inver
      getinver <- function() inv
      list(set = set, get = get,
           setinver = setinver,
           getinver = getinver)
}
######################################################

## The following function calculates the inverse of the 
## input real, square, invertible matrix x. 
## First,the function checks if the inverse has already 
## been calculated. If that is true, it gets the inverse  
## from the cache and skips the computation. Else, it 
## calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinver function.

cacheSolve <- function(x, ...) {
      inv <- x$getinver()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinver (inv)
      inv
}