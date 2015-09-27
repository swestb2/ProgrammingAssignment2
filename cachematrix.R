## Below are two functions to create a special object that stores a matrix and 
##caches its inverse.


## The first function, makeCacheMatrix creates a special matrix, which is really
##a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setinverse <-function(solve) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, 
        getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so it gets the inverse from the cache and skips
## the computation. Otherwise it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
        
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  
}
