## This set of functions takes an invertible matrix x and computes its inverse.
## If the inverse has already been computed, the functions return the cached inverse
## without repeating the computation.

## makeCacheMatrix(mtrx) prepares an invertible matrix to be inverted and cached.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # sets i to 0
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    get <- function() x # returns the matrix as is
    setinv <- function(inv) i <<- inv # sets i to be equal to the inverse of the matrix
    getinv <- function() i # retrieves the value stored as i - the matrix inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv) # returns a list of the parameters set in this function
}

## cachesolve(makeCacheMatrix(mtrx)) returns the inverse of the previously defined matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv() # checks if matrix inverse i has already been computed 
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get() # computes inverse of matrix 
    i <- solve(data, ...) 
    x$setinv(i) # assigns inverse to variable i for future retrieval
    i
}
