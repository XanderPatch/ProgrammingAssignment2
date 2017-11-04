## These functions create a cache of an inverse of a matrix and store it
## in cache

## makeCacheMatrix is a vector of functions used to get/set the value of the 
## matrix in cache and get/set the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(invert) inv <<- invert
                getinv <- function() inv
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}


## cacheSolve takes a matrix, checks if it is in cache, then either returns it
## or if not found, solves it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getinv()
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinv(invert)
        return(invert)
}


