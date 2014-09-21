## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "vector", 
# which is really a list containing a function to

# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse
# 4. get the value of the inverset


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# Return a matrix that is the inverse of 'x'
# If the cached inverse is available, cacheSolve retrieves it, while if
# not, it computes, caches, and returns it
cacheSolve <- function(x, ...) {
  # return the inverse from the cache if it has been cached already
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
  }
  inv_x
}