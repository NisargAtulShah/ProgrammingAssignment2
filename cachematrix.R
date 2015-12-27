######## This code stores the inverse of the matrix in the cache
######## so that if the inverse is required later, it need not be computed again.

## the first function "makeCacheMatrix" returns a list, 
## which stores the following functions:
## set - initializes the matrix with the data and its inverse with a null value
## get - calls the matrix
## setsolve - calcultes the inverse of a matrix.
## getsolve - calls the inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
               x <<- y
               m <<- NULL
               }
          get <- function() x
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m
          
          list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
}


## the second function "cacheSolve" returns the inverse of the matrix, 
## it returns the previously calculted value (if available), or computes the inverse

cacheSolve <- function(x, ...) {
               m <- x$getsolve()
               if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
                    }
               data <- x$get()
               m <- solve(data, ...)
               x$setsolve(m)
               
               m
}
