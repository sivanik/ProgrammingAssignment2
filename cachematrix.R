## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(solve) i <<- solve
  getInvMatrix <- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInvMatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setInvMatrix(i)
  i
}

