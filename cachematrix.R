# makeCacheMatrix creates a list of functions to set and get value of the matrix and
# get, set vale of the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cachesolve function resturns the inverse of the matrix. It checks if the inverse
# was calculated earlier, if yes then it takes rhe result from the cache
cacheSolve <- function(x) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Extracting from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

