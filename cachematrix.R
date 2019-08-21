## The following functions show the power of lexical scoping for complex computing operations like 
## inverse of a matrix 
## 

## The first function creates a special matrix which is really a list containing a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.get the value of the matrix inverse
##4.set the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set,
       get = get,
       getinv = getinv,
       setinv = setinv)
}


## The following function calculates the inverse of the  matrix created with the above function. However, 
##it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, 
##it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("retrieving from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
