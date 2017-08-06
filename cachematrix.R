## makeCacheMatrix and cacheSolve are intended to work together to 
## eliminate redundant calls to the solve function.  This is accomplished
## by using a cache.

## Constructs an object capable of storing a matrix and a cached value. 
## 
## x should be of type matrix
## returns a list of functions:
##    get: returns x
##    setcache: allowing caller to set the cache value
##    getcache: allowing caller to access the cache value

makeCacheMatrix <- function(x = matrix()) {
   c <- NULL
   set <- function(y) {
      x <<- y
      c <<- NULL
   }
   get <- function() x
   setcache <- function(cache) c <<- cache
   getcache <- function() c
   list(set = set, get = get,
        setcache = setcache,
        getcache = getcache)
}


## Finds the inverse of x using the solve function.  If solve has already been 
## calculated on x, a cached value is returned.
##
## x must be instance of makeCacheMatrix
## ... arguments will be passed through to the solve function

cacheSolve <- function(x, ...) {

   if(class(x) == "matrix") {
      stop("atomic vectors or matrix not valid.  Use makeCacheMatrix to construct valid matrix")
   }
   
   c <- x$getcache()
   if(!is.null(c)) {
      message("getting cached data")
      return(c)
   }
   data <- x$get()
   c <- solve(data, ...)
   x$setcache(c)
   c
}