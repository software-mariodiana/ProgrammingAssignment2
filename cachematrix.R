## 
# Function: makeCacheMatrix
#
# Given a matrix, return a list object that allows us to cache and retrieve both 
# the matrix and its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL

    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    get <- function() x

    setsolve <- function(solve) s <<- solve

    getsolve <- function() s

    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


##
# Function: cacheSolve
#
# Given the list data structure created by the above function, compute and return the 
# inverse of the matrix contained within, using any cache that exists.
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()

    if (!is.null(s)) {
        message("Getting cached data...")
        return(s)
    }

    data <- x$get()

    s <- solve(data, ...)
    x$setsolve(s)
    s
}
