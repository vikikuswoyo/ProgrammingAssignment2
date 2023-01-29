## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### The first function, makeCacheMatrix creates a special "matrix", which is
### really a list containing a function to
###  -set the value of the matrix
###  -get the value of the matrix
###  -set the value of the inverse of matrix
###  -get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inverse <<- inverse
  getinverse <- function() x_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
### The following function calculates the inverse of the special "matrix" created 
### with the above function. However, it first checks to see if the inverse has 
### already been calculated. If so, it gets the inverse from the cache and skips 
### the computation. Otherwise, it calculates the inverse of the data and sets 
### the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setinverse(x_inverse)
  x_inverse
}
