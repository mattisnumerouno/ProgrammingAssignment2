## This pair of functions solves for the inverse of a matrix and then caches the value using superassignment.
## This value is then returned using the second function

## This function sets the objects for the cachesolve function and handles superassignment

makeCacheMatrix <- function(x=numeric()) {
  i <- NULL
  get <- function() {x}
  setinv <-function(inverse) {i <<- inverse}
  getinv <- function() {i}
  list( get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function returns a cached value if it's there, and solves for the inverse of object x if it isn't

cachesolve <- function (x, ...){
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
