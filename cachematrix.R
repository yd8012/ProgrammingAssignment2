## Implemented inverse-cacheable matrix

## makeCacheMatrix
## make a custom inverse-cacheable matrix
## Parameter
## x : matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Given x should be matrix
        ## Also, it should be square.
        if (!inherits(x, "matrix") | dim(x)[1] != dim(x)[2]) {
                stop("makeCacheMatrix must be provided with the square matrix")
        }

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}

## cacheSolve
## Return inverse of the matrix.
## If cache exists, return the cache value.
## Else, do the calculation - save it to the cache and return.
## Parameter
## x : custom cachable-matrix

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
