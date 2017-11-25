## This pair of functions can be used to create a special matrix object 
## to which is possible to set and to get is inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setsolve <- function(inverted) inv_m <<- inverted
        getsolve <- function() inv_m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has already 
## been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_m <- x$getsolve()
        
        if(!is.null(inv_m)) {
                message("getting cached matrix")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setsolve(inv_m)
        inv_m
        
}
