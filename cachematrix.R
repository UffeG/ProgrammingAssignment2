## Compute the inverse of a matrix and cache the result
## Assignment 2 of the Coursera R course

## Function makeCacheMatrix
## Creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the value of the inverse matrix
## 4.	get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  InvM <- NULL
  set <- function(y) {
    x <<- y
    InvM <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) InvM <<- inv
  getinverse <- function() InvM
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheInverse
## Calculates the inverse of the special "matrix" created with the above function. 
## First check to see if the inverse has already been calculated. If so, get the inverse from the cache.
## If cache is empty then calculate the inverse of the data and set the cache via the setinverse function.

cacheInverse <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getinverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setinverse(invM)
  invM
}
