## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: This function places the inverse of a matrix into a cache to obviate the need to recalculate the inverse each time.

## It creates a special "matrix", which is really a list containing a function to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function takes the out of the MakeCacheMatrix function and calculates the inverse of the matrix that was the input for the MakeCacheMatrix

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
    
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix.data <- x$get()
  inv <- solve(matrix.data, ...)
  x$setinv(inv)
  return(inv)
}
