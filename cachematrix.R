## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix and creates a list that contains accessor methods for that matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set_matrix <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve takes a cacheMatrix from the method above, andchecks for an inverse of 
## the matrix stored in the cache and returns it. If no inverse was cached, it calculates,
## caches and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get_matrix()
    inv <- solve(matrix)
    x$set_inverse(inv)
    inv
}
