## Make a list of functions which can be used to find the inverse of a matrix
## and caches this inverse for later use (when cacheSolve function is used)
makeCacheMatrix <- function(x = matrix()) {
  ## Argument: a matrix which can be inverted
  ## Return: a list of functions which can be used by the function
  ##         cacheSolve to find the inverse of a matrix, and to cache this
  ##         inverse for later use
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Finds the inverse of a matrix; if the matrix of a particular
## variable name has already been solved, it searches the answer in the
## makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Argument: makeCacheMatrix of a matrix, assigned to a 
  ##           variable name passed as the argument x
  ## Return:   a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}