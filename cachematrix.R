## Coursera Course: R Programming
## Programming Assignment 2: Caching the Inverse of a Matrix
## This assignement contains two functions: makeCacheMatrix & cacheSolve
## 
## ##############################################################################
## Function: makeCacheMatrix
## Parameter: x is a matrix
##  * Creates the static variable used to cache
##  * Declares four inner functions: set, get, setinverse, getinverse
##  * The list command creates a list that pairs names with the functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(mx_inverse) m <<- mx_inverse
  getinverse <- function() m
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## ###################################################################################################################
## Function: cacheSolve
## Parameter: x (a matrix) & ... (arguments passed down to another function)
##  * Call to getinverse() returns the value of "m" in the 
##  * If "m" has a value it is the cached inverse matrix; report the cached value and exit function
##  * If the value of "m" is NULL; get() the matrix; solve the inverse; store the inverse matrix in the global "m"

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
