## The following code is for two functions used to calculate the inverse of a matrix
##(which is a resourse intensive process if done repeatedly).

##The goal of this exercise is to make sure that if a matrix has been inversed once, 
##that inverse matrix will be stored in a different environment for later use, further   
##requests for inversion must make use of this cached value.

## The first function makeCacheMatrix, makeVector creates a special list, which 
##is really a list containing functions to:

##1) set the value of the matrix
##2) get the value of the matrix
##3) set the value of the inverse of the matrix
##4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y= matrix()) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse<-function(solve) {
    m <<- solve
  }
  
  getinverse <- function() m
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

##The following function calculates the inverse of the matrix supplied in special list 
##created with the above function. However, it first checks to see if 
##the inverse has already been calculated. If so, it gets the inverse from 
##the cache and skips the computation. Otherwise, it calculates the 
##inverse of the data and sets the value of the inverse in the cache via 
##the setmean function.


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
