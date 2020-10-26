## Put comments here that give an overall description of what your
## functions do

# the functions below can combine to evaluate the inverse of a matrix
# while first checking if the inverse has not already been evaluated
# if the inverse has already been evaluated and is in the cache, this 
# cached inverse is returned, rather than recalculating the inverse matrix


## Write a short comment describing this function

# this function creates a list that contains four functions; these functions
# can be called to set the the matrix being evaluated, retrieve this matrix,
# set the inverse of this matrix, and retrieve the inverse of this matrix;

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# this function uses the output of makeCacheMatrix to first determine if the
# inverse of a specific matrix has already been evaluated and is stored in 
#the cache; if it has, the already-calculated inverse is returned, rather 
# than recalculated; if it is not in the cache, the inverse of the matrix is 
# calculated using the source() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}
