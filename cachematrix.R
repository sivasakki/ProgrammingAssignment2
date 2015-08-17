## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Initializing m
## Setting the data
## Getting the data
## Setting the inverse matrix to m
## Getting the inverse of matrix from m
## List of functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

## Getting Saved Inverse to m
## If the inverse is available then get from cache and display message "getting cached data"
## If the inverse is not available in the cache, get the data and use "solve" to make inverse of matrix and set to m
## return m

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## Use this to test this function to see if we get the message getting cached data

## Type this in the console 
## source("cachematrix.R") 
## invMatrix(makeCacheMatrix(rbind(c(1,-1/4),c(-1/4,1))))
## if you see the messsage "getting cached data", then the cache works! 
invMatrix=function(x){
    cacheSolve(x)
    cacheSolve(makeCacheMatrix(rbind(c(1,-1/2),c(-1/2,1))))
    cacheSolve(x)
}

## Adding SHA1