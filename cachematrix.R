## Matrix inverse calculations can take a lot of computation power
## it is beneficial to cache the value of a calculation if one has to
## compute it over and over.

## makeCacheMatrix creates a special "matrix object"
## that can Cache it's inverse
## First we set the value for the matrix
## Next we get the value of the matrix
## then we set the value of the inverse of the matrix
## lastly we get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix from makeCache matrix (using Solve(x))
## if the inverse has been computed already (and the matrix hasn't changed)
## then the routine cacheSolve will get the inverse from Cache. If the matrix
## changes, or inverse hasn't been computed, it will compute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## the matrix passed in has to be invertible
        ## (solve() function only works on square, invertible matrices)
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
