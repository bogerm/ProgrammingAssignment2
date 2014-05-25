## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(mat) {
      x <<- mat
      im <<- NULL
    }
    get <- function() x
    setinv <- function(solve) im <<- solve
    getinv <- function() im
    list(
      set = set,
      get = get,
      setinv = setinv,
      getinv = getinv
      )
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinv()
        if (!is.null(im)) {
          message("getting cached matrix")
          return(im)              # Return inverted matrix
        }
        om <- x$get()
        im <- solve(om,...)
        x$setinv(im)
        im                        # Return inverted matrix
}
