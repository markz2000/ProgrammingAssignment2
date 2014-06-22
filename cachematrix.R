##==============================================================================
## This function creates a special "matrix" object that can cache its inverse.
##==============================================================================

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(Inverse) m <<- Inverse
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)      ## <-- returns the list of functions
     
}

##=========================================================================================
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##========================================================================================

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if(!is.null(m)) {
          message("Getting cached matrix")
          return(m)
     }
     data <- x$get()
   ##   m <- solve(data)
     
     ##  Send message if matrix is not invertible
     result = tryCatch({
          m <- solve(data)
     }, warning = function(w) {
          stop("Matrix is not invertible")
     }, error = function(e) {
          stop("Matrix is not invertible")
     }, finally = {
          stop
     })
     
     x$setInverse(m)
     m
}
