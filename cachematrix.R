## This program cache the inverse of  matrix, by using 
## two functions: makeCacheMatrix and cacheSolve.
## It is assumed that the matrix is real, square, 
## and invertible. If the matrix is singular 
## the program will terminate with an error message.
## This program is compiled with: source ("cachematrix.R")
###############################################################

## Function: makeCacheMatrix
###############################################################
## The input of  this function  is a real, square, 
## invertible matrix. 
## Return:
##  A list with the following four functions:
## set: set the value of the matrix
## get: get the value of the matrix
## setinver:  set the inverse of the matrix
## getinver: get the inverse of the matrix

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
###############################################################

## Function: cacheSolve
###############################################################
## Computes the inverse of a real, square, invertible 
## matrix. 
## Return:  
## If the inverse has already been computed,returns
## the inverse.  Else, the inverse of the matrix 
## is computed, the value of the inverse is set 
## in the cache via the setinver function, and the 
## inverse of the matrix is returned with a message.
## If the matrix is singular, that is, not invertible
## the following error message is returned: 
## Error in solve.default(data, ...) : 
## Lapack routine dgesv: system is exactly singular.

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

###############################################################
## Example 1: 
## Real, square, invertible, matrix. 
## > mat <- makeCacheMatrix(matrix (c(5,4,3,2,5,5,3,6,7), nrow = 3, ncol =3))
## > cacheSolve (mat)
## [,1]  [,2]  [,3]
## [1,]  0.25  0.05 -0.15
## [2,] -0.50  1.30 -0.90
## [3,]  0.25 -0.95  0.85
## > cacheSolve (mat)
## getting cached data
## [,1]  [,2]  [,3]
## [1,]  0.25  0.05 -0.15
## [2,] -0.50  1.30 -0.90
## [3,]  0.25 -0.95  0.85

## Example 2:
## Checking the inverse using matrix multiplication
## A matrix time its inverse must be equal to the unit matrix
## > mat1 <- makeCacheMatrix (matrix (c(5,4,3,2,5,5,3,6,7), nrow = 3, ncol =3))
## > ma1 <- matrix (c(5,4,3,2,5,5,3,6,7), nrow = 3, ncol =3)
## > ma1%*%cacheSolve (mat1)
## [,1] [,2] [,3]
## [1,]  1.000000e+00    0    0
## [2,] -2.220446e-16    1    0
## [3,]  0.000000e+00    0    1

## Example 3:
## A real, singular matrix:
## > mat <- makeCacheMatrix (matrix (1:9, nrow = 3, ncol =3))
## > invma <- cacheSolve (mat)
## Error in solve.default(data, ...) : 
##      Lapack routine dgesv: system is exactly singular: U[3,3] = 0 