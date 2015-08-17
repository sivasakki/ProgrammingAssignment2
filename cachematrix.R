## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

invMatrix=function(x){
    cacheSolve(x)
    cacheSolve(makeCacheMatrix(rbind(c(1,-1/2),c(-1/2,1))))
    cacheSolve(x)
}
