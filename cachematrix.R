## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix takes matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix.
#The operators <<- and ->> are normally only used in functions, and cause a search to be made through parent environments for an existing definition of the variable being assigned

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL #null matrix created
  set <- function(y) {
    x <<- y #assign new matrix and null matrix created again
    inverse_matrix <<- NULL 
  }
  get <- function() x
  setInverse <- function(inverse) inverse_matrix <<- inverse
  getInverse <- function() inverse_matrix #output inverse_matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data") #return msg if inverse has been computed
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
