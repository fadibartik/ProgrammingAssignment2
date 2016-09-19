##  The code below create a pair of functions that can cache the inverse of a matrix.

## makeCacheMatrix: This function generates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
                      x <<- y
                      m <<- NULL
                     }
        get <- function() x
        setinvmatrix <- function(invmatrix) m <<- invmatrix
        getinvmatrix <- function() m
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
                                            }

## cacheSolve will compute the inverse of the special "Matrix" computed by makeCacheMatrix. 
## Cachesolve will return the inverse from the cache, if the the matrix has not changed (and calculated)

cacheSolve <- function(x, ...) {
                                m <- x$getinvmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
                    }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}
