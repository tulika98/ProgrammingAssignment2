## These two function generate or retrieve (from a cache) the inverse of a matrix

## This function accepts a matrix as argument and create cache to store the matrix and its inverse. 
##It contains different function to create or return matrix or the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL                        
  set <- function(b) {                          
    x <<- b
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function accepts a matrix as argument and computes the inverse of a matrix (assuming matrix is always invertible).
## If the inverse is already computed than it  does not compute the inverse but retrive 
## the inverse from the cache data

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("Retrieving previously cached data.")
    return(inverse_matrix)
  }
  dat <- x$get()
  inverse_matrix <- solve(dat)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
