## These functions compute the inversion of a given
## matrix. The last result of the inversion operation
## is cached so that following requests for the inverse
## of the same matrix are not recomputed, but the cached
## result is returned instead.

## Creates a special matrix type that can cache the result
## of its inversion

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    
    get_matrix <- function() {
        x
    }
    set_matrix <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get_inverse <- function() {
        inv_m
    }
    set_inverse <- function(inv) {
        inv_m <<- inv
    }
    
    list(get_matrix = get_matrix,
         set_matrix = set_matrix,
         get_inverse = get_inverse,
         set_inverse = set_inverse)
}


## Computes the inverse of a matrix making use
## of the function defined above

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message("Got cached data")
        return(inv)
    }
    
    mat <- x$get_matrix()
    inv <- solve(mat, ...)
    x$set_inverse(inv)
    
    inv
}
