
##
## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## makeVector <- function(x = numeric())
  
   m <- NULL
   set <- function(y) {
   x <<- y
   m <<- NULL
   }
   get <- function() x
   setMatrix <- function(solve) m <<- solve
   getMatrix <- function() m
   list(set = set, get = get,
        setMatrix = setMatrix,
        getMatrix = getMatrix)  
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
## Return a matrix that is the inverse of 'x'
  
   m <- x$getMatrix()
   
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   
   matrix <- x$get()
   m <- solve(matrix, ...)
   x$setMatrix(m)
   
   m  
}
