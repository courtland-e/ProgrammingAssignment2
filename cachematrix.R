## This file contains two functions (makeCacheMatrix and cacheSolve)
## that cache the inverse of a matrix

## Creates a matrix object that can cache inversions of itself

makeCacheMatrix <- function(x = matrix()) {
    ## same as vector-cacheing example 
    i <- NULL ##set to NULL until new instance is initiated
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {i <<- inverse}
    getinverse <- function() {i}
    ## inner function values added to list
    list(set=set, get=get,setinverse=setinverse,
         getinverse=getinverse)
}


## Computes the inverse of the matrix object returned by
## makeCacheMatrix, and if inverse has already been cached, merely
## retrieves and returns the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    ## if i isn't NULL, prints message and returns cached value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
