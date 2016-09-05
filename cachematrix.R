# makeCacheMatrix creates a list of functions to store the matrix and cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  #gets the matrix value
  setinv <- function(inverse) inv <<- inverse  #sets inverse value of the matrix, saves cache
  getinv <- function() inv #gets cache
  list(set = set, get = get, setinv = setinv, getinv = getinv) # generates a list of functions
}

# cacheSolve function returns the inverse of the matrix. It checks if the inverse
# was calculated earlier, if yes then it takes the result from the cache

cacheSolve <- function(x) {
  inv <- x$getinv()  #get the inverse and store into "inv"
  if (!is.null(inv)) {     #check if cache exits or not
    message("Extracting from cache") 
    return(inv)    #returns the inverse stores in the cache
  }
  data <- x$get()   #gets the new matrix
  inv <- solve(data)  #performs inverse operation on the new matrix
  x$setinv(inv) 
  inv # returns the inverse 
}

