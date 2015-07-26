## Put comments here that give an overall description of what your
## functions do
## The two functions below are a pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
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
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ## Checking if a cached data exists
        if(!is.null(m)){
                ## Cached data found, message back
                message("getting cached data")
                ## return this function
                return(m)
        }
        ## Cached data not found
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
