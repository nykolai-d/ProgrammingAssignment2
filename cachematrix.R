## This function creates a matrix and caches its inverse. 

makeCacheMatrix <- function(m = matrix()) {
      invMatrix <- NULL
      set <- function(n) {
            m <<- n
            invMatrix <<- NULL
      }
      get <- function() m
      setInverse <- function(inverse) invMatrix <<- inverse
      getInverse <- function() invMatrix
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## The next function computes the inverse of the matrix returned by the previous function.
## If the inverse has already been calculated, the information is retrieved from the cache.
## allowing a more optimal computation process.

cacheSolve <- function(m, ...) {
      ## Returns the inverse matrix of the matrix m
      invMatrix <- m$getInverse()
      if (!is.null(invMatrix)) {
            message("Retrieving data")
            return(invMatrix)
      }
      n <- m$get()
      invMatrix <- solve(n, ...)
      m$setInverse(invMatrix)
      invMatrix
}

# Testing the functions:
col1 <- c(3,5)
col2 <- c(4,6)
matrix_1 <- cbind(col1, col2)
matrix_1
m <- makeCacheMatrix(matrix_1)
inverse <- cacheSolve(m)
inverse
