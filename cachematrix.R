## The following functions create a matrix object that can cache
## its inverse, and allow you to calculate and cache the inverse of that
## matrix.

## makeCaheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # initially, there is no cached inverse
    i <- NULL

    # set method sets matrix values and resets inverse to NULL (uncached)
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    # get method just returns the internal matrix
    get <- function() x

    # setinverse method stores the inverse in the cached
    # matrix object so it can be retrieved later
    setinverse <- function(inverse) i <<- inverse

    # getinverse method jsut returns the stored inverse value,
    # no calculations performed to do this.
    getinverse <- function() i

    # return internals when called without method
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve calculates and caches the inverse if a cached inverse
## does not exist, otherwise it returns the cached value

cacheSolve <- function(x, ...) {
  # request cached inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    # not null, so return cached inverse
    message("getting cached data")
    return(i)
  }
  # otherwise, get matrix and calculate inverse
  data <- x$get()
  i <- solve(data, ...)
  # now cache that inverse for later use and return it
  x$setinverse(i)
  i
}
