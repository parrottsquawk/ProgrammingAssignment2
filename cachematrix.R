## These functions together provide the ability to calculate the inverse of a matrix
## and cache the results without requiring additional computation. This allows for
## future operations to be discounted to an O(1) 'get' function.

## This function accepts a standard matrix object and returns a cacheMatrix with
## getter and setter functions which are used to retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function takes in a cacheMatrix and calculates its inverse; the inverse is then cached
## inside the original matrix for future recall without further calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
