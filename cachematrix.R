## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix create a matrix object with several methods that allows to access the
## inverse of the matrix, in case this inverse has been previously loaded.
## Function returns a list with all the methods to set and get the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL
  set <- function(y) {
    x <<- y
    inverse<<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse<<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculate the inverse of an object matrix (created previously with makeCacheMatrix), and
## in case the inverse has been previously calculated this inverse which is stored on the function closure of the matrix object is recovered and return

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse<- solve(data, ...)
  x$setinv(inverse)
  inverse
}
