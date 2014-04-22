# The following two functions greatly speed the re-computation of inverting a 
# matrix by caching the matrix and the inversion result. Subsequent calls for 
# the inverted matrix will return the cached result rather than having to
# recompute from the original matrix.
# 
# Usage: First pass a matrix object to makeCacheMatrix and assign the result.
# Then pass the result to cacheSolve. cacheSolve will compute the inverse IF one
# has not already been computed.

# Assume that the matrix supplied is always square and invertible.

# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinvmat <- function(inv) invmat <<- inv
    getinvmat <- function() invmat
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}


# cacheSolve: This function computes the inverse of the
# special "matrix" returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache. 


cacheSolve <- function(x, ...) {
    invmat <- x$getinvmat()
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data, ...)
    x$setinvmat(invmat)
    invmat
}
