## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  #inv_x stores the cached inverse matrix
  inv_x <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverse matrix
  setinverse <- function(inverse) inv_x <<- inverse
  
  #get the value of the inverse matrix
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
      message("getting cached data")
      return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinverse(inv_x)
    inv_x
  }
