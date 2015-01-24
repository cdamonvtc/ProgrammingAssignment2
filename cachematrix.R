## this is a pair of functions to avoid re-calculating the inverse of a matrix every time it is needed

## create (or wrap) a matrix so that it can hold onto its inverse as well

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
    	x <<- y
    	inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) {
    	inv <<- inverse
    }
    getInverse <- function() inv
    list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
}


## get the inverse for the cached matrix, computing the inverse if needed

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
        	message("re-using cached inverse")
        	return (inv);
        }
        mx <- x$get()
        inv <- solve(mx)
        x$setInverse(inv)
        inv
}
