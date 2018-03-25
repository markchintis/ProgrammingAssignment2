## This first function will create a new special matrix and cache its inverse. 
## The second function computes the inverse of the special matrix made by makeCacheMatrix and returns the value. If it is already found, then the function returns the cached inverse.

## This program will create a new special matrix 

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inv(data, ...)
  x$setinv(inv)
  inv
}
