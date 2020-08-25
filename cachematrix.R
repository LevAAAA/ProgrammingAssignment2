## functions to improve computation time for repeated calculation of inverse
## using caching mechanism to store matrix


## function creating object with properties to set/get values/properties 

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


## create inverse matrix and get inverse matrix from cache if already exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(is.null(i)){
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
  }
  i
}
