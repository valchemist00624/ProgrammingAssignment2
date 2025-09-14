## Creating a pair of functions that cache the inverse of a matrix.

#Creating a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  wow <- NULL
  #Setting the matrix
  set <- function(y){
    x <<- y
    wow <<- NULL
  }
  #Getting the matrix
  get <- function() x
  #Setting the inverse of the matrix
  setInverse <- function(inverse_s) wow <<- inverse_s
  #Getting the inverse of the matrix
  getInverse <- function() wow
  #Returning a list of methods
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#Computing the inverse of the matrix created above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  wow <- x$getInverse()
  if(!is.null(wow)){
    message("getting cached data")
    return(wow)
  }
  #get the matrix from our object
  mat <- x$get()
  #Calculating the inverse of the matrix
  wow <- solve(mat, ...)
  #Setting the inverse of the object
  x$setInverse(wow)
  #Returning the matrix
  wow
}
