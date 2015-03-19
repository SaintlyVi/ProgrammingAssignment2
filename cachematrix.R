## This set of functions takes an invertible matrix x and computes its inverse.
## If the inverse has already been computed, the functions return the cached inverse
## without repeating the computation.

## makeCacheMatrix(mtrx) prepares an invertible matrix to be inverted and cached.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cachesolve(makeCacheMatrix(mtrx)) returns the inverse of the previously defined matrix. 

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