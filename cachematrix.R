## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {##get a matrix as an input
  matinv <- NULL #initialize the inverse matrix variable to NULL as soon as a Matrix is created using makeCacheMatrix
  set <- function(y) { ##Set function to set the values of global environment variables
    x <<- y ##Reset the the vlue of the matrix in the gloal environment
    matinv <<- NULL ##Reset the the value of the inverse matrix in the gloal environment to NULL
  }
  get <- function() x ## Get the value of the matrix set recently
  setinv <- function(inv) matinv <<- inv ##Set the the value of the inverse matrix in the gloal environment
  getinv <- function() matinv ## ## Get the inverse matrix set recentl
  list(set = set, get = get,setinv = setinv,getinv = getinv)## Return a list of functions used to get or set the principla or inverse matrix 
}

cacheSolve <- function(x, ...) {##Take a list of functions returned from makeCacheMatrix as an input
  matinv <- x$getinv() ##Get the inverse matrix stored in the global environment and stoer it in function environment 
  if(!is.null(matinv)) {##If the cache has the inverse matrix, it implies that the principle matrix has not been reset else the matinv in the global env would have been set to NULL
    message("getting cached data")
    return(matinv) ##Return the cached matrix(inverse)
  } else {
    data <- x$get()##Get the matrix stored in the global env by makeCachedMatrix and store it in the function env
    matinv <- solve(data) ##Solve the recently obtained matrix to get its inverse and store it in the function env variable for storing the inverse matrix
    x$setinv(matinv)} ##Set the global environment variable for inverse matrix with the recently obtained inverse matrix stored in the function environment
  matinv ##Return and auto-print the inverse matrix stored in the function environment
}
