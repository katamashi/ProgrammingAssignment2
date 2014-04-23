## In line with function makeVector, the first function, makeCacheMatrix creates a special matrix,
## a list containing a function to

## set the matrix (set)
## get the matrix (get)
## set the inverse of the matrix (setsolve)
## get the inverse of the matrix (getsolve)

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


## Following cachemean: function inverts the special matrix 
## created with makeCacheMatrix. However, it first checks to see 
## if the inverse has already been created. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the inverse in the cache 
## via the setsolve function.

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