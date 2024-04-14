## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix creates a special object which caches its inverse and provides
##methods to set and get the matrix value along with the cached inverse.
##cacheSolve computes inverse of matrix stored and caches the result if not cached already
##and returns inverse.

## Write a short comment describing this function

##The function creates a special object called 
##matrix with the ability to cache its inverse.
##It takes an argument x which can be a matrix.There are four inner functions inside.
##set: It sets the value of the matrix.
##get: It retrieves the current value of matrix stored.
##setInverse: It sets the cached inverse of the matrix.
##getInverse: It retrieves the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <-NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
get <-function() x
setInverse <-function(inverseMatrix) inverse <<-inverseMatrix
getInverse <-function() inverse
list(set = set, get = get,setInverse = setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

##Function computes inverse of matrix stored in the matrix x created above by
##makeCacheMatrix
##It retrieve the cached inverse and returns it.
##If the inverse is not cached, it calculates the inverse using solve function.
##After computation, it caches the result using setInverse.
##Then it returns computed inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <-x$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setmean(inverse)
  
  inverse
}
