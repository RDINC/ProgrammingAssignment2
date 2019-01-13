## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#>Purpose is to create a function that can breate a matrix,
#which can cache its inverse for an input. 


makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x 
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

##This is designed to inverse the matrix
##usingmakeCacheMatrix above. Once the inverse is 
##calculated, the cache solve will retrieve the inverse
##from cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting Cached Invertible Matrix") 
    return(invMatrix)
  }
  MatrixData <- x$getMatrix()
  invMatrix <- solve(MatrixData, ...)   
  x$setInverse(invMatrix) 
  return(invMatrix)
  
}


