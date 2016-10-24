## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly
## This script does Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { x }
  getinverse <- function() { i }
  setinverse<- function(inv) { i <<- inv }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by above function
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Example used for testing
## > x<-makeCacheMatrix(matrix(c(0.6, -0.2,-0.7, 0.4), nrow=2, ncol=2))

##  > x$get()
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## First time getting inverse, no cache available
## > cacheSolve(x)
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6

## Second time getting inverse, its get from cache
## > cacheSolve(x)
## getting cached data
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6

## Third time getting inverse, again getting from cache
## > cacheSolve(x)
## getting cached data
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
 
