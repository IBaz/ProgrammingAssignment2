## Put comments here that give an overall description of what your
## functions do
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1: set the value of the matrix
## 2: get the value of the matrix
## 3: set the value of the inverse matrix
## 4: get the value of the inverse matrix

##It has a few conditions "if", I used it for checking possibility of building a square matrix,
## i.e. we can build a square matrix when length of input vector is squared integer
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      if(length(x) == 0) {
            message("Input is empty")
      } else {
            x_order <- sqrt(length(x))
            if(x_order %% floor(x_order) != 0) {
                  message("Can't build a square matrix: the length of the input vector must be the square of an integer")
                  stop()
            }
            x <- matrix(x, x_order, x_order)
      }
      set <- function(y) {
            y_order <- sqrt(length(y))
            if(y_order %% floor(y_order) != 0) {
                  message("Can't build a square matrix: the length of the input vector must be the square of an integer")
                  stop()
            }
            x <<- matrix(y, y_order, y_order)
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
