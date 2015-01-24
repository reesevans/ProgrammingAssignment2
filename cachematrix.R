## These functions provide a way to cache (save) the inverse of
##  a matrix in order to prevent unnecessary recalculation
##  of the inverse, a O(n^3) operation.

##  makeCacheMartrix, creates a function which retains
##  the matrix data and its inverse. It also provides accessor functions
##  to set and get both the matrix and it's inverse. 
##
##  Usage: mat<- makeCacheMatrix(dat) 
##  This implementation assumes dat is an invertible matrix.
##  cacValues are accessed by the
##  sub-functions set, get, setinv and getinv. 
##  If mat$getinv() returns NULL
##  then the inverse has not yet been calculated and a call
##  cacheSolve is required.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(ymat) {
            x <<- ymat
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      
}

## Usage: cacheSolve(mat) 
## cacheSolve(mat) returns the matrix inverse of mat, either from the cache or 
## by calculation if the cache is NULL
## cacheSolve(mat) prints a message if mat$getinv() is not NULL to indicate
## that the inverse is retrieved from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
