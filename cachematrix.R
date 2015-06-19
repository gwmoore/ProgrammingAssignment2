### Example: Caching the Inverse of a Matrix

## In this example we introduce the `<<-` operator which can be used to
## assign a value to an object in an environment that is different from the
## current environment. Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      set_inv_mtrx <- function(usr_input) m <<- usr_input
      get_inv_mtrx <- function() m
      list(set = set, get = get,
           set_inv_mtrx = set_inv_mtrx,
           get_inv_mtrx = get_inv_mtrx)
}



## The following function 'cacheSolve' calculates the inverse of the special "matrix"
## returned by 'makeCacheMatrix' above. However, it first checks to see if the
## inverse has already been calculated. If so, and the matrix has not changed, it
## get`s the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the cache via the
## 'set_inv_mtrx' function.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$get_inv_mtrx()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$set_inv_mtrx(m)
      m
}
