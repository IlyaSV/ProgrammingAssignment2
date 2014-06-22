## Functions to operate matrices, which can cache its inverse at 
## first compute.

## Creates special matrix object, which can cache inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve<- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Return inverse matrix. Cache it to skip computing next time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
