## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix will create a matrix object with
# various helper methods (set, get, setinverse, getinverse)
# so that external functions (ie. cacheSolve) can interact with it
# it returns a list object, with the list containing the helper functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # this will eventually contain the value of the inverse
  set <- function(y) { # this is a way to set your matrix to a different value
    x <<- y
    m <<- NULL # if the value changes, the inverse should be reset to null  and will need to be recomputed
  }
  get <- function() x # simply displays the value of 'x', the matrix
  setinverse <- function(mean) m <<- mean # sets whatever value is passed to it to the inverse
  getinverse <- function() m # simply displays the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# takes a makeCacheMatrix object and checks if the inverse already exists.
# if it does, it displays it and prints "getting cached data".
# if it does not exist yet, it calculates it and then uses the setinverse helper
# method to cache the value for the future.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
