## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function accept a matrix and store it in memory
## it will set the variable Y as the value when x$set() is called
## calling get will return x
## setinverse will store the inverse value in memory
## getinverse will retrieve the stored value from memory
## the return function will be the list of stored value and its functions
makeCacheMatrix <- function(x = matrix()) {
  
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


## Write a short comment describing this function
## Write a short comment describing this function
## this function will attempt to get the inverse value from memory
## if there is no data in memory it will proceed to inverse the matrix and store the result in memory and return the inverse result
## if there is data found in memory, a message will be displayed and the stored data will be displayed
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
