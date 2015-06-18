## Put comments here that give an overall description of what your
## functions do

## Creates a list of matrix functions that allow the object of the function to 
## calculate its inverse, and if it has been already calculated, cache it. 
## setm and setinv allocate the values of the matrix and its inverse accordingly
## getm and getinv retreive the values of the matrix and its inverse accordingly

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  setm <-function (mat){
    x <<- mat
    inv <<- matrix()
  }
  getm <- function() x
  
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  
  list(setm = setm, getm = getm,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of a matrix and if it has been already calculated, 
## retreives it from memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!anyNA(inv) ) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getm()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
