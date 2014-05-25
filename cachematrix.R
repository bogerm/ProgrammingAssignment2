## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
