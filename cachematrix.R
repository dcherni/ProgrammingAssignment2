## Combined these functions calculate the inverse of a matrix and cache it so 
## that the inverse can be directly accessed later without having to recalulate it.

## makeCacheMatrix function creates a list object whose 4 elements are functions 
## that can be applied against an input matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mtrx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtrx <<- NULL
  }
  get <- function() x
  set_inv <- function(z) inv_mtrx <<- z
  get_inv <- function() inv_mtrx
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## cacheSolve calculcates the inverse of the matrix passed to it and 
## caches that inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mtrx <- x$get_inv()
  if(!is.null(inv_mtrx)) {
    message("getting cached data")
    return(inv_mtrx)
  }
  mtrx <- x$get()
  inv_mtrx <- solve(mtrx)
  x$set_inv(inv_mtrx)
  inv_mtrx
}
