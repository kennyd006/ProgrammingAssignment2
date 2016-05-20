## Together, the below functions return the inverse of a square matrix either by
## calculating the inverse if it has not already been calculated or by retrieving 
## the inverse from the cache if it has already been calculated

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function either calculates or retrieves the inverse of the "matrix" object from the 
## makeCacheMatrix function above

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}

